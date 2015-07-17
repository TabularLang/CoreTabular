namespace MicrosoftResearch.Infer.Tabular.TaskPane
module TableNorm =

    open TableType


    /// Helper types and functions
    module TableNormUtil =
        /// all combinations of a list of size size
        /// adapted from http://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f
        let rec combinations acc size list = seq {
            match size, list with 
            | n, x::xs -> 
                if n > 0 then yield! combinations (x::acc) (n - 1) xs
                if n >= 0 then yield! combinations acc n xs 
            | 0, [] -> yield acc 
            | _, [] -> () }

        /// helper function to get a subset of values from an array in order
        let getVals vals (arr:'a[]) =
            Array.map (fun idx -> arr.[idx]) vals

        /// used for smashing together array of objects into a string with a designated separator between object strings
        let smashIntoString sep =
            Array.map (fun o -> if o = null then "" else o.ToString()) // WHAT TO DO FOR MISSING VALUES?
            >> Array.reduce (fun o1 o2 -> o1.ToString() + sep + o2.ToString() )

        let attachIndices arr = Array.mapi (fun idx v -> (idx,v)) arr

    open TableNormUtil


    /// is the candidate domset truly a domset for the rngidxs?
    /// Define *functional dependency* as the columns in rngset always taking the same value whenever the columns in domset have the same value
    /// Yes ==> return a map of domset to the range indexes
    /// No ==> return None
    let getDomSet tdata domset rngset =
        // loop through the rows of data
        // check: is the domset already in the fdmap?
        // no ==> put it in
        // yes ==> check if values in rngset match
            // no ==> return None
            // yes ==> ok; go to next row
        Seq.fold (fun fdmapopt objarr ->
                    match fdmapopt with
                    | None -> None
                    | Some (fdmap:Map<'a[],'a[]>) ->
                        let domvals = objarr |> getVals domset
                        let rngvals = objarr |> getVals rngset
                        match domvals |> fdmap.TryFind with
                        | None -> Some <| Map.add domvals rngvals fdmap
                        | Some rnginmap ->  if rnginmap = rngvals
                                            then Some fdmap
                                            else None // terminates the map construction; not a functional dependency
                 ) (Some Map.empty) tdata

    /// Get the minimum cardinality set of column indices that *functionally determine* column rngcolidx.
    /// Return None if the minimum cardinality set is all other columns
    let getMinDomSet tdata rngcolidx =
        if Seq.isEmpty tdata
        then None
        else
            /// helper function; return the first domset of size domsetsize from the list of column indices in allowedColumns
            /// also returns the actual mapping
            let getFirstDomSet domsetsize allowedColumns =
                combinations [] domsetsize allowedColumns
                |> Seq.tryPick 
                    (List.toArray >> (fun domset -> 
                                        match getDomSet tdata domset [| rngcolidx |] with
                                        | None -> None
                                        | Some fdmap -> Some (domset,fdmap)
                    ))
                // returns None if no combination worked

            /// get total number of columns
            let numcols = tdata |> Seq.head |> Array.length
            if rngcolidx < 0 || rngcolidx >= numcols then invalidArg "rngcolidx" (sprintf "out of range; #cols is %d" numcols)
        
            // start with sets of size numcols-2 and keep decreasing the size until we find a domset or hit size 0
            match
                Seq.init (numcols-2) ((-) (numcols-2))
                |> Seq.tryPick (fun domsetsize -> // try every combination of columns excluding the rngcol to see if it is a domset
                                getFirstDomSet domsetsize [for i in 0..(numcols-1) do if i <> rngcolidx then yield i]
                               ) with
            | None -> None           // no domsetsize worked
            | Some (domset,fdmap) -> // we have a domset; is there a smaller one?
                // for each size smaller than the current size and larger than 0
                // try all subsets of domset of that size
                // if none of the subsets work, indicate it with a boolean false ==> signal to stop the fold
                let rec getSmallestDomset domset fdmap =
                    match  getFirstDomSet (Array.length domset - 1) (List.ofArray domset) with
                    | None -> domset, fdmap
                    | Some (domsetSmaller,fdmapSmaller) -> getSmallestDomset domsetSmaller fdmapSmaller
            
                Some <| getSmallestDomset domset fdmap

    /// Given a domset of columns that functionally determine firstrng, the range index, augment the fdmap with
    /// all columns that the domset functionally determines
    let getMaxRngSet tdata domset firstrng fdmap =
        if Seq.isEmpty tdata
        then [|firstrng|],fdmap
        else
            /// get total number of columns
            let numcols = tdata |> Seq.head |> Array.length
            if firstrng < 0 || firstrng >= numcols then invalidArg "firstrng" (sprintf "out of range; #cols is %d" numcols)

            // for each column not in domset and <> firstrng,
            //  build an fdmap for that column.  If successful, augment fdmap
            // return the rngset and the final fdmap
            [ for i in 0..(numcols-1) do if Array.forall ((<>) i) domset && i <> firstrng then yield i ]
            |> Seq.choose (fun col ->
                            match getDomSet tdata domset [| col |] with
                            | None -> None
                            | Some m -> Some (col,m)
                          )
            //|> Seq.append (Seq.singleton (firstrng,fdmap))
            |> Seq.fold (fun (rngmap,fdmap) (col,m) ->
                            Array.append rngmap [| col |],
                            Map.fold (fun mapAccum k v -> 
                                        let oldv = Map.find k mapAccum
                                        let newv = Array.append oldv v
                                        Map.add k newv mapAccum
                                     ) fdmap m
                        
                        ) ([|firstrng|],fdmap)

    /// Returns the best choice for domset, rngset, fdmap.
    /// Postcondition Guarantees: 
    ///     domset is smallest possible;
    ///     rngset is largest possible given domset;
    ///     domset union rngset do not encompass the whole table (all columns) [marked as (!) in algorithm]
    ///         (intuition: we're using this method to normalize the table. Including all columns will not reduce data redundancy.
    ///             Instead, it indicates a viable primary key.)
    ///     domset is not of size 0, which indicates a column is constant for all rows [marked as (*) in algorithm]
    ///         (ideally we optimize out such columns; we don't need to include them at all)
    let getBestFD tdata =
        MAYBE {
            do! if Seq.isEmpty tdata then None else Some ()
            /// get total number of columns
            let numcols = tdata |> Seq.head |> Array.length
        
            // get first candidate solutions
            let! (domsetFull,rngsetFull,fdmapFull,col1) = 
                [ 0..(numcols-1) ] // candidate first rngcol
                |> List.tryPick (fun col -> 
                                    MaybeExpression() {
                                        let! domset1, fdmap1 = getMinDomSet tdata col
                                        let rngsetFull, fdmapFull = getMaxRngSet tdata domset1 col fdmap1
                                        do! if rngsetFull.Length + domset1.Length = numcols then None else Some () // (!)
                                        do! if domset1.Length = 0 then None else Some() // (*)
                                        return domset1, rngsetFull, fdmapFull, col
                                    }
                                )
            /// see if one of the rng cols has a minDomSet smaller than the current one
            let rec getBestRec dset r1 rset fdmap =
                match
                    rset
                    |> Array.tryPick (fun c ->
                                        MaybeExpression() {
                                            do! if c = r1 then None else Some ()
                                            let! ds, fdm0 = getMinDomSet tdata c
                                            do! if ds.Length >= Array.length dset then None else Some ()
                                            let rs, fdm1 = getMaxRngSet tdata ds c fdm0
                                            do! if rs.Length + ds.Length = numcols then None else Some () // (!)
                                            do! if ds.Length = 0 then None else Some() // (*)
                                            return getBestRec ds c rs fdm1
                                        }
                                     ) with
                | None -> dset,rset,fdmap
                | Some tup -> tup
        
            return getBestRec domsetFull col1 rngsetFull fdmapFull
        }

    /// allows a column to be considered in the domain or range of a FD
    let columnFilterFD colinfo =
        match colinfo.coltype with
        | Bool 
        | Link _ 
        | Discrete _ -> true
        | _ -> false
    
    /// wrapper around getBestFD, only considering columns that pass columnFilterFD
    let getBestFD_Restricted table = 
        MAYBE {
            do! if Seq.isEmpty table.data then None else Some ()
            let allowedColumns = table.colinfos |> Array.map columnFilterFD
            let origIdxArr = allowedColumns |> attachIndices |> Array.filter snd |> Array.map fst
            let dataSubset = table.data |> Seq.map (Array.zip allowedColumns >> Array.filter fst >> Array.unzip >> snd)
            let! domset, rngset, fdmap = getBestFD dataSubset
            let origDomset = domset |> Array.map (Array.get origIdxArr)
            let origRngset = rngset |> Array.map (Array.get origIdxArr)
            return origDomset, origRngset, fdmap
        }

    /////////////////////////////////// now make methods to do Table Normalization
    /// Recur on the list of tables in tableDTO
    let rec normalize tableDTO =
        match tableDTO.tables with
        | [] -> tableDTO
        | origTable::rest ->
            match getBestFD_Restricted origTable with // NOTE: columns restriction
            | None ->                                                         // no more tables to create for this table
                let resultDTO = normalize {tableDTO with tables=rest} // create additional tables for the remainder
                {resultDTO with tables=(origTable:: resultDTO.tables)}        // return this table with the rest
            | Some (domset, rngset, fdmap) ->                           // can split origTable into table1::table2
                assert (domset.Length <> 0)
                //assert (domset.Length + rngset.Length < numcols)

                let moreOneDomset = domset.Length > 1
                //case when we have >= 2 columns in domset
                // create a new table table1
                // create a new column generated by concatenating the columns in domset together with a space in between
                //let origTableWithDomCol = origTable.addDerivedCol (getVals domset >> smashIntoString " "  >> (fun s -> upcast s)) newcolinfo

                /// Just the column name in the domset-size-1 case.  "col1_col2_col3" in the multi-domset case.
                let domcolname = origTable.colinfos |> getVals domset |> Array.map (fun colinfo -> colinfo.colname) |> smashIntoString "_"
                let table1name = "T_" + domcolname
                /// First new table, formed from domset and rngset
                let table1 =
                    // copy over the original table domset and rngset columns, preceded by the concatenated domcol
                    let table1colinfos = 
                        Array.concat (seq{
                            if moreOneDomset then yield [| {colname=domcolname; coltype=String} |]
                            yield origTable.colinfos |> getVals domset
                            yield origTable.colinfos |> getVals rngset 
                        })
                    let table1data :TableData = 
                        fdmap 
                        |> Map.toSeq 
                        |> Seq.map (fun (domarr,rngarr) ->
                                        Array.concat (seq{
                                            if moreOneDomset then yield [| domarr |> smashIntoString " " :> System.IComparable|]
                                            yield domarr
                                            yield rngarr
                                        })
                                    )
                    {name=table1name; colinfos=table1colinfos; data=table1data}
                /// Second new table, formed from the original minus the domset, plus the domsetColumn, minus the rngset
                /// Make the domsetColumn a link to table1
                /// for just one domcol, keep the original position.  For two or more, put the domsetColumn at the end
                let table2 =
                    let table2colinfos =
                        Array.concat (seq{
                        if moreOneDomset
                        then
                            for (idx,colinfo) in attachIndices origTable.colinfos do
                                if domset |> Array.forall ((<>) idx)     // no column in domset
                                    && rngset |> Array.forall ((<>) idx)    // no column in rngset
                                then yield [| colinfo |]
                            yield [| {colname=domcolname; coltype=Link(table1name, domcolname)} |] // last column
                        else
                             for (idx,colinfo) in attachIndices origTable.colinfos do
                                if domset |> Array.forall ((=) idx)     // no column in domset, if more than one col in domset
                                then yield [| {colname=domcolname; coltype=Link(table1name, domcolname)} |]
                                elif rngset |> Array.forall ((<>) idx)    // no column in rngset
                                then yield [| colinfo |]
                        })
                    let table2data :TableData = 
                        origTable.data
                        |> Seq.map (fun objarr ->
                                    // first gather the domsetcol value
                                    let domcolval = objarr |> getVals domset |> smashIntoString " " :> System.IComparable
                                    // find the row this corresponds to in table1
                                    let domcolrowidx = (table1.getRowIdx domcolname domcolval).Value
                                
                                    Array.concat (seq{
                                        if moreOneDomset
                                        then
                                            for (idx,o) in attachIndices objarr do
                                                if domset |> Array.forall ((<>) idx)        // no column in domset
                                                    && rngset |> Array.forall ((<>) idx)    // no column in rngset
                                                then yield [| o |]
                                            yield [| domcolrowidx |] // last column
                                        else
                                            for (idx,o) in attachIndices objarr do
                                                if domset |> Array.forall ((=) idx)        
                                                then yield [| domcolrowidx |]             // yield the new row link in place
                                                elif rngset |> Array.forall ((<>) idx)    // no column in rngset
                                                then yield [| o |]
                                    })
                                   )
                    {origTable with colinfos=table2colinfos; data=table2data}

                // Scratch Proof: no column in domset or rngset has an incoming link (is the primary key of some relationship)
                // Case a col in domset is a primary key another table points to
                //      --> that means that col is all unique values
                //      the col must functionally determine all other column values
                //      therefore, there is no way col could be in the domset 
                //      *contradiction*
                // Case a col in rngset is a primary key another table points to
                //      --> that means that col is all unique values
                //      Same as before; it cannot be in the rngset *contradiction*
                // Other columns linked to ok.
                // Moral: no relationships need updating
                let domnames = origTable.colinfos |> getVals domset |> Array.map (fun colinfo -> colinfo.colname)
                let rngnames = origTable.colinfos |> getVals rngset |> Array.map (fun colinfo -> colinfo.colname)
                assert(
                        tableDTO.relations 
                        |> Set.forall (fun rel -> 
                                        fst rel.primary <> origTable.name                       // primary key is not this table 
                                        || (Array.forall ((<>) (snd rel.primary)) domnames      // or (not one of the columns in domset
                                            && Array.forall ((<>) (snd rel.primary)) rngnames)) //     AND not one of the columns in rngset)
                )

                // Add in new relationships
                let newrels = Set.add {primary=(table1name,domcolname); foreign=(origTable.name,domcolname)} tableDTO.relations 

                // the newly created table1 and table2 may need further normalizing, recur with them included
                normalize {tableDTO with tables=table1::table2::rest; relations=newrels}



        //                                (fun objarr -> 
        //                                    let valarr = getVals domset objarr
        //                                    valarr |> smashIntoString " "
        //                                ) 





    module (*private*) TableNormTest =
        /// col0 functionally determines col1
        let data1:TableData = Seq.init 5 (fun idx -> [|idx; idx%2; 42|])         
        /// col0 does NOT functionally determine col1
        let data2:TableData = Seq.append data1 (Seq.singleton [| 1; 0; 42 |])

        // testing
        assert(getDomSet data1 [| 0 |] [| 1 |] = Some (Map.ofList [([|0|], [|0|]); 
                                                                    ([|1|], [|1|]); 
                                                                    ([|2|], [|0|]); 
                                                                    ([|3|], [|1|]);
                                                                    ([|4|], [|0|])] ))
        assert(getDomSet data1 [| 1 |] [| 0 |] = None)
        assert(getDomSet data1 [|  |] [| 2 |] = Some ( Map.ofList [([||], [|42|])] ) )
        assert(getDomSet data2 [| 0 |] [| 1 |] = None)
        assert(getDomSet data2 [| 1 |] [| 0 |] = None)

        let domsetData1, fdmapData1 = (getMinDomSet data1 1).Value
        assert(Some (domsetData1, fdmapData1) = Some ([| 0 |], Map.ofList [([|0|], [|0|]); 
                                                                    ([|1|], [|1|]); 
                                                                    ([|2|], [|0|]); 
                                                                    ([|3|], [|1|]);
                                                                    ([|4|], [|0|])] ))
        assert(getMinDomSet data1 2 = Some ([| |],  Map.ofList [([||], [|42|])] ) ) // the case of a constant column

        let rngsetData1, fdmapData1Full = getMaxRngSet data1 domsetData1 1 fdmapData1
        assert ( rngsetData1 = [|1; 2|] )
        assert (fdmapData1Full = Map.ofList [([|0|], [|0; 42|]); ([|1|], [|1; 42|]); ([|2|], [|0; 42|]);
                                             ([|3|], [|1; 42|]); ([|4|], [|0; 42|])] )

    
        //assert( getBestFD data1 = Some ([||], [|2|], Map.ofList [([||], [|42|])]) )
        //assert( getBestFD data2 = Some ([||], [|2|], Map.ofList [([||], [|42|])]) ) // taken out
        let data3 = Seq.append data1 (Seq.singleton [| 1; 1; 43 |])
        assert( getBestFD data3 = Some ([|0|], [|1|],Map.ofList 
                                                           [([|0|], [|0|]); ([|1|], [|1|]); ([|2|], [|0|]); ([|3|], [|1|]);
                                                            ([|4|], [|0|])]) )
        let data4 = Seq.append data1 (Seq.singleton [| 1; 0; 43 |])
        assert( getBestFD data4 = None )
        assert( getBestFD (Seq.singleton [| "a"; "b" |] ) = None )
        let data5 = seq{
                        yield [| "a"; "b"; "g" |] 
                        yield [| "b"; "c"; "f" |]
                    }
        assert( getBestFD data5 = None )
        let data6 :TableData = seq{
                        yield [| "a"; "b"; "g" |] 
                        yield [| "b"; "c"; "f" |]
                        yield [| "a"; "c"; "f" |]
                    }
        assert( getBestFD data6 = Some ([|2|], [|1|], Map.ofList [([|"f"|], [|"c"|]); ([|"g"|], [|"b"|])]) )
        
        let data6table1 =
            let colinfos =
                [|  {colname="c1"; coltype=Discrete 2};
                    {colname="c2"; coltype=Discrete 2};
                    {colname="c3"; coltype=Discrete 3};
                |]
            {name="data6table1"; colinfos=colinfos; data=data6}
        assert( getBestFD_Restricted data6table1 = Some ([|2|], [|1|], Map.ofList [([|"f"|], [|"c"|]); ([|"g"|], [|"b"|])]) )

        let data6table2 =
            let colinfos =
                [|  {colname="c1"; coltype=Discrete 2};
                    {colname="c2"; coltype=Discrete 2};
                    {colname="c3"; coltype=String};
                |]
            {name="data6table2"; colinfos=colinfos; data=data6}
        assert( getBestFD_Restricted data6table2 = None )

        let data7 :TableData = seq{
                        yield [| "a"; "b"; "g"; "g" |] 
                        yield [| "b"; "c"; "f"; "f" |]
                        yield [| "a"; "c"; "f"; "f" |]
                    }
        let data7table3 =
            let colinfos =
                [|  {colname="c1"; coltype=Discrete 2};
                    {colname="c2"; coltype=Discrete 2};
                    {colname="c3"; coltype=String};
                    {colname="c4"; coltype=Discrete 2}
                |]
            {name="data7table3"; colinfos=colinfos; data=data7}
        assert( getBestFD_Restricted data7table3 = Some ([|3|], [|1|], Map.ofList [([|"f"|], [|"c"|]); ([|"g"|], [|"b"|])]) )

        let GSTableDTOOrig =
            let colinfos =
                [|   {colname="G"; coltype=String};
                    {colname="S"; coltype=String};
                    {colname="Prio"; coltype=String};
                    {colname="Cites"; coltype=Bool};
                    {colname="Country"; coltype=String};
                    {colname="Price"; coltype=Real}
                |]
            let tabledata :TableData = 
                seq {
                    yield [| "ga"; "sa"; "H"; true; "FR"; 8.33 |]
                    yield [| "ga"; "sa"; "H"; true; "UK"; 15.20 |]
                    yield [| "ga"; "sa"; "H"; true; "FR"; 7.00 |]
                    yield [| "ga"; "sb"; "M"; true; "FR"; 5.50 |]
                    yield [| "ga"; "sb"; "M"; true; "FR"; 7.00 |]
                    yield [| "ga"; "sb"; "M"; true; "UK"; 8.50 |]
                    yield [| "ga"; "sb"; "M"; true; "USA"; 5.01 |]
                    yield [| "gb"; "sa"; "L"; false; "FR"; 5.60 |]
                    yield [| "gb"; "sa"; "L"; false; "USA"; 2.99 |]
                    yield [| "gc"; "sc"; "L"; true; "UK"; 10.00 |]

                    yield [| "gc"; "sd"; "L"; false; "FR"; 1.00 |]
                    yield [| "gd"; "sa"; "L"; false; "USA"; 0.99 |]
                    yield [| "gd"; "sa"; "L"; false; "USA"; 5.01 |]
                    yield [| "ga"; "se"; "M"; true; "FR"; 8.33 |]
                    yield [| "gb"; "se"; "L"; true; "FR"; 2.00 |]

                    yield [| "gb"; "se"; "L"; true; "USA"; 2.00 |] //
                    yield [| "ga"; "sa"; "H"; true; "USA"; 2.00 |]
                    yield [| "gc"; "sc"; "L"; true; "UK"; 2.00 |]
                    yield [| "gb"; "sa"; "L"; false; "UK"; 2.00 |]
                    yield [| "gb"; "sa"; "L"; false; "UK"; 3.00 |]
                }
            let table = {name="cool_table"; colinfos=colinfos; data=tabledata}
            {tables=[table]; relations=Set.empty}

        let bestFD = (GSTableDTOOrig.tables |> List.head).data |> getBestFD 

        let a = normalize GSTableDTOOrig
//        (a.tables |> List.head).data |> Seq.toList
//        (a.tables |> List.tail |> List.head).data |> Seq.toList
        // RESULT: // can't do equality comparison because Seq does not support structural equality testing (it tests by reference)
        let b =
          {tables =
            [{name = "T_S_G";
              colinfos =
               [|{colname = "S_G";
                  coltype = String;}; {colname = "S";
                                       coltype = String;}; {colname = "G";
                                                            coltype = String;};
                 {colname = "Prio";
                  coltype = String;}; {colname = "Cites";
                                       coltype = Bool;}|];
              data =
               List.toSeq
                 [[|"sa ga"; "sa"; "ga"; "H"; true|]; [|"sa gb"; "sa"; "gb"; "L"; false|];
                   [|"sa gd"; "sa"; "gd"; "L"; false|]; [|"sb ga"; "sb"; "ga"; "M"; true|];
                   [|"sc gc"; "sc"; "gc"; "L"; true|]; [|"sd gc"; "sd"; "gc"; "L"; false|];
                   [|"se ga"; "se"; "ga"; "M"; true|]; [|"se gb"; "se"; "gb"; "L"; true|]
                  ]};
             {name = "cool_table";
              colinfos =
               [|{colname = "Country";
                  coltype = String;}; {colname = "Price";
                                       coltype = Real;};
                 {colname = "S_G";
                  coltype = Link ("T_S_G", "S_G");}|];
              data =
               Seq.ofList
                 [[|"FR"; 8.33; 0|]; [|"UK"; 15.2; 0|]; [|"FR"; 7.0; 0|]; [|"FR"; 5.5; 3|];
                   [|"FR"; 7.0; 3|]; [|"UK"; 8.5; 3|]; [|"USA"; 5.01; 3|]; [|"FR"; 5.6; 1|];
                   [|"USA"; 2.99; 1|]; [|"UK"; 10.0; 4|]; [|"FR"; 1.0; 5|]; [|"USA"; 0.99; 2|];
                   [|"USA"; 5.01; 2|]; [|"FR"; 8.33; 6|]; [|"FR"; 2.0; 7|]; [|"USA"; 2.0; 7|];
                   [|"USA"; 2.0; 0|]; [|"UK"; 2.0; 4|]; [|"UK"; 2.0; 1|]; [|"UK"; 3.0; 1|] 
                  ]}
             ];
           relations = Set.singleton {primary = ("T_S_G", "S_G");
                                        foreign = ("cool_table", "S_G");} }
        
        // Idea: add a restriction function that eliminates the FDs we don't want
            // one for column selection -- only select Bool, Integer*, Real*, String (* = small cardinality = # unique values)
            // one for the particular -- no [||] domain set