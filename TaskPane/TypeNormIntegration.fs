namespace MicrosoftResearch.Infer.Tabular.TaskPane

open MicrosoftResearch.Infer.Tabular.DataLayer.DBSchema
open TableType
open TableNorm
//open System
open MicrosoftResearch.Infer.Tabular.StringToInferNet // DTO stuff

module TypeNormIntegration =

    /// convert a system type to mytype.  This function can use the actural table data, but it does not at the moment
    /// the Type is one of Int32, Boolean, double, Single, DateTime, string
    let convertToOldColumnType (sysType:System.Type) _colData = // we can include the whole dbtable if we need it
        match sysType with
        | x when x = typeof<System.Int32> -> OInteger
        | x when x = typeof<System.DateTime> -> ODateTime
        | x when x = typeof<System.Boolean> -> OBool
        | x when x = typeof<System.Single> 
              || x = typeof<double> -> OReal
        | x when x = typeof<string> -> OString
        | _ -> failwithf "unexpected type %A" sysType

    /// From the Excel Data Model of the given Workbook
    let getDataModelData (wb:Microsoft.Office.Interop.Excel.Workbook) =
        use loader = new MicrosoftResearch.Infer.Tabular.DataLayer.DataModelConnectors.DataModelLoader(wb :?> _, false) :> MicrosoftResearch.Infer.Tabular.DataLayer.NewIO.ILoader
        let schema = loader.GetSchema()
        let (log,err,(schema,_)) = MicrosoftResearch.Infer.Tabular.Elaborator.elaborate(schema) //crusso: type the schema (ignoring errors)
        let _, dicLog, dicIdStrategy, dicDatas, _,  _, idToPos = MicrosoftResearch.Infer.Tabular.Service.readTable loader schema 
        DTO(dicDatas)

    /// Convert a DTO (typically from Excel data model) into the TableType type system.
    let convertTableDTO (dbgraph:DBEntityGraph) (DTO(dicDatas)) =
        /// recursive helper function.
        /// Create a list of tables in topological order, using the data, column types and relations.
        let rec sortTablesTopo tablesRemain =
            if Array.isEmpty tablesRemain
            then    []
            else    let nextTableIdx = 
                        match tablesRemain 
                              |> Array.tryFindIndex (fun t -> 
                                    let tableDependencies = dbgraph.InEdges(t).CastSeq()
                                    // no dependence of t may reside in a future table
                                    Seq.forall (fun (tdep:DBRelation) -> tablesRemain |> Seq.forall (fun trem -> trem <> tdep.Source) ) tableDependencies
                                   ) with
                        | Some x -> x
                        | None -> failwithf "dependence issue; none of the tables in %A have no dependence on a future table\n" tablesRemain
                    let (tname:TableName), dbtable = tablesRemain.[nextTableIdx] 
                    let tableData = dicDatas |> Map.find tname
                    let colinfos, cds = 
                        dbtable.fields.CastSeq() 
                        |> Array.ofSeq
                        |> Array.map (fun kvp -> 
                                    let (cname:ColumnName), col = kvp.Key, kvp.Value
                                    let colIdx = tableData |> fst |> Map.find cname
                                    let colData = tableData |> snd |> Seq.map (flip Array.get colIdx) |> Seq.cast<System.IComparable>
                                    let colinfo =
                                        match col with
                                        | Remote ((ftname:TableName),(fcname:ColumnName)) ->
                                            {colname=cname; coltype=Link(ftname,fcname)}
                                        | Local (systyp,_) ->
                                            let coltyp = convertToOldColumnType systyp colData |> convertToNewType colData
                                            {colname=cname; coltype=coltyp}
                                    colinfo, colData |> Seq.map (Array.create 1)
                                    )
                        |> Array.unzip
                    let td = cds |> Array.reduce (Seq.map2 Array.append)
                    {name=tname; colinfos=colinfos; data=td} 
                    //:: sortTablesTopo (Array.append tablesRemain.[..(nextTableIdx-1)] tablesRemain.[(nextTableIdx+1)..] )
                    :: sortTablesTopo (if nextTableIdx > 0 then Array.append tablesRemain.[..(nextTableIdx-1)] tablesRemain.[(nextTableIdx+1)..] 
                                                           else tablesRemain.[(nextTableIdx+1)..])
        let tables =          
            dbgraph.Vertices.CastSeq() |> Array.ofSeq |> sortTablesTopo
        
        let relations =
            dbgraph.Edges.CastSeq() 
            |> Seq.map (fun (e:DBRelation) -> //(DBRelation(ftable:TableName,ptable:TableName,{ForeignName=fcol:ColumnName; Name=pcol:ColumnName})) ->
                        let ptable :TableName = fst e.Source
                        let ftable :TableName = fst e.Target
                        let {ForeignName=fcol:ColumnName; Name=pcol:ColumnName} = e.join
                        {primary=(ptable,pcol); foreign=(ftable,fcol)})
            |> Set.ofSeq
        {tables=tables; relations=relations}



    open Microsoft.Office.Interop
    open MicrosoftResearch.Infer.Tabular.DataLayer.ExcelNames

    /// go to the leftmost column that has enough empty cells to hold numRows rows (and numCols columns wide)
    let getNextEmptyRange (ws:Excel.Worksheet) numRows numCols = 
        (Seq.skipWhile (fun (r:Excel.Range) -> not r.allBlank)
                      (Seq.initInfinite (fun idx -> ws.Range(cte (idx+1,1), cte (idx+numCols + 1,numRows)) )) // try starting from column 1, 2, 3, ... until whole range can fit
        |> Seq.head).selectSubRng(topcol=2) // extra padding of one column 
    
    /// write a single table to a sheet
    let writeTable (ws:Excel.Worksheet) table =
    //(*(wb:Excel.Workbook)*) (ws:Excel.Worksheet) //(tname:TableName) ((colmap,data) as dn:dataNormalized) :Excel.ListObject = 
        // does a ListObject already exist? Yes --> delete it and overwrite
        try
            let lo = ws.ListObjects.Item(table.name) // throws exception if table does not exist

            // if the table exists, first delete all worksheet connections from this table // connections not considered here
            //getConnectionsForTable wb tname |> List.iter (fun con -> con.Delete())
        
            let rColname = lo.Range.selectSubRng(botrow=1).Offset(-1,0)
            rColname.Clear() |> ignore
            let rColtype = rColname.Offset(-1,0)
            rColtype.Clear() |> ignore
            lo.Delete()
        with 
            | (*:? System.Runtime.InteropServices.COMException as*) e -> () // table does not exist

        // how many rows and columns do we need?
        let nC = table.colinfos.Length
        let nR = Seq.length table.data + 3 // number of actual rows + column label row + table label row + column type row
        // get fresh range
        let rng = getNextEmptyRange ws nR nC
        // first write the table name
        do  
            let tableNameCell = rng.Item(1,1):?>Excel.Range
            tableNameCell.Value2 <- table.name //.selectSubRng(toprow=1,botrow=1)
            tableNameCell.Font.Bold <- true
        // write column names and column types
        do
            let colRng = rng.selectSubRng(toprow=2,botrow=3)
            Array.iteri (fun idx colinfo -> 
                            colRng.selectSubRng(topcol=(idx+1),botcol=(idx+1)).Value2 <- array2D [| [|colinfo.coltype.ToString()|]; [|colinfo.colname|] |]
                            colRng.selectSubRng(toprow=2,botrow=2,topcol=(idx+1),botcol=(idx+1)).Font.Italic <- true
                        ) (table.colinfos)
        // write table values
        do
            let dataRng = rng.selectSubRng(toprow=4)
            Seq.iteri (fun idx objarr -> 
                            let subrng = dataRng.selectSubRng(toprow=(1+idx),botrow=(1+idx)(*,topcol=1,botcol=(Array.length objarr)*))
                            let oarr = (Array.map (fun o -> o:>obj) objarr)
                            subrng.Value2 <- oarr
                            ()
                      ) (table.data)
        // make a new ListObject
        let tableRng = rng.selectSubRng(toprow=3)
        let lo = ws.ListObjects.Add(SourceType=Excel.XlListObjectSourceType.xlSrcRange,
                                    Source=tableRng,
                                    XlListObjectHasHeaders=Excel.XlYesNoGuess.xlYes)
        lo.Name <- table.name
        //lo.DisplayName <- tname
        lo.Comment <- "Table "+table.name+"; autogenerated by Tabular ModelWizard"
        lo

    
    
        

    /// Apends the tables in a TableDTO to a Worksheet.
    /// Overwrites/replaces tables with the same name.
    /// Returns a list of newly created ListObjects.
    let appendTableDTO (ws:Excel.Worksheet) tableDTO =
        tableDTO.tables |> List.map (writeTable ws)


    /// Synchronizes the tableDTO to the Worksheet so that the Worksheet exactly reflects the tables in the tableDTO.
    /// DELETES all data in the Worksheet and starts from a fresh, empty one.
    /// Returns a list of all ListObjects created.
    let syncTableDTO (ws:Excel.Worksheet) tableDTO =
        ws.Cells.Clear()|> ignore                    // delete all data
        appendTableDTO ws tableDTO

