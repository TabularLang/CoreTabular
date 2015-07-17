namespace MicrosoftResearch.Infer.Tabular.DataLayer
open Microsoft.Office.Interop.Excel
open Microsoft.Office.Interop
open MicrosoftResearch.Infer.Tabular
open System.Linq
open System.Data.OleDb
open System
open System.IO
open ADODB
open System.Data.OleDb
open System.Data.SqlClient
open NewIO

[<AutoOpen>]
module DataModelConnectors= 
  open Syntax
  open ExcelNames
  open System.Collections

  let uniqueAndDuplicate xs = 
      Seq.fold(fun ((unique, duplicate))  e ->  if Set.contains e unique
                                                then (unique, Set.add e duplicate)
                                                else (Set.add e  unique, duplicate))
               (Set.empty, Set.empty)
               xs

  let getTable (wb : Excel.Workbook) tableName =  
      try wb.Model.ModelTables.Item(tableName)
      with |e -> failwith (sprintf "could not find %A in the PowerPivot datamodel, please make sure it the name is correct"  tableName)


  type DataModelLoader(wb : Excel.Workbook, openPPForBug: bool) as this = 
      let fromRelations (tableName) = 
         let relations = getRelations wb |> Seq.groupBy(fun e -> e.PrimaryKeyTable.Name) |> Map.ofSeq
         relations.TryFind(tableName) |> Option.map (fun v -> v.First().PrimaryKeyColumn.Name)



      interface NewIO.ILoader with 
          member x.InferIDStrategy (tableName:TableName) suppliedID = 
               let table = getTable wb tableName
               let columnSet = table.ModelTableColumns.Cast<ModelTableColumn>() |> Seq.map(fun e -> e.Name) |> Seq.toArray
               findIDHeuristic tableName columnSet  suppliedID (fromRelations tableName)


          member x.ReadConcrete (tableName:TableName) idStrategy (colNames:ColumnName list) =
            try 
              wb.Model.Initialize()
              let adoConnection = wb.Model.DataModelConnection.ModelConnection.ADOConnection :?> ADODB.Connection
              let table = getTable wb tableName
              let query = sprintf "evaluate '%s' " tableName + match idStrategy with |FromColumn(idColumn) ->  sprintf " ORDER BY %s "  ( "'" + tableName + @"'[" + idColumn + @"]" ) | _ -> ""
              let rs = adoConnection.Execute(query )
              
              let extractColumnName tableName input = 
                let regexMatch = System.Text.RegularExpressions.Regex.Match(input, tableName + "\[(.+)\]");
                regexMatch.Groups.[1].Value;
             
              let fieldNametoPos = 
                  rs.Fields.Cast<Field>() 
                  |> Seq.toArray 
                  |> Array.map(fun f -> extractColumnName tableName f.Name)
                  |> Array.mapi (fun i e -> e,i) |> Map.ofSeq

              let ider = match idStrategy with | FromPosition -> let i = ref -1 in (fun _ -> i := !i+1; !i :> IComparable)
                                               | FromColumn(idColumn) -> (fun (elem:obj[]) -> elem.[fieldNametoPos.[idColumn]] |> box :?> IComparable)

              let data = rs.GetRows() :?> obj[,] 
              do data |> Array2D.iteri(fun i j e -> if System.Convert.IsDBNull(e) then data.[i,j] <- null) //fields * records

              let base1, height1 = (Array2D.base1 data), (Array2D.length1 data) //fields
              let base2, width2  = (Array2D.base2 data), (Array2D.length2 data) //records
              let dataWithId = seq{ for i2 in 0 .. width2 - 1 ->
                                        let elem = Array.init height1 (fun j1 -> data.[base1 + j1, base2 + i2])
                                        ider elem, elem } |> Seq.toArray

              (fieldNametoPos, dataWithId |> Array.toSeq)
            with |e -> raise  e

          member x.GetSchema () = let io = (x :> NewIO.ILoader)
                                  let fId = (fun tableName ->  io.InferIDStrategy tableName None |> fst)
                                  let fUniqueValues = (fun tableName fieldName ->  let (cols, data) = io.ReadConcrete tableName  (fId tableName) [fieldName]
                                                                                   let ci = cols.[fieldName]
                                                                                   //let (unique,duplicate) = uniqueAndDuplicate (data |> Seq.map fst)
                                                                                   let unique =  
                                                                                       data |> Seq.fold (fun S (id,e) ->
                                                                                                         let v = e.[ci] 
                                                                                                         if v = null then S else Set.add (v:?>IComparable) S)
                                                                                                        Set.empty 
                                                                                      
                                                                                   unique)
                                  let fHasNoEmptyValue  = (fun tableName fieldName () ->  let (cols, data) = io.ReadConcrete tableName  (fId tableName) [fieldName] 
                                                                                          let ci = cols.[fieldName]
                                                                                          data |> Seq.forall (fun (id,e) -> e.[ci] <> null))
                                  getSchema wb fId fUniqueValues fHasNoEmptyValue

      interface IDisposable with  member x.Dispose() =  ()




    type DicData = Map<TableName, Map<ColumnName,int> * (obj array seq)>

    type Microsoft.FSharp.Collections.Map<'K, 'V when 'K : comparison > with 
        member t.Keys = t |> Seq.map(fun e -> e.Key)


    //type DataSink<'Src> = TableName -> int *  (ColumnName -> 'Src[])
    [<AutoOpen>]
    module ExcelActions = 
        let tabularPosteriorTagName =  "tabular_posterior_next"
        let outputrange = "tabular_output" 
        let dataTag ws = ensureTag ws (1,9) "tabular_datastart"
        let postTag ws = ensureTag ws (1,5) tabularPosteriorTagName

        let validNamedRange (name:Name    )=  not( (name.RefersTo :?> string).Contains("#REF!"))
        let validNamedTable (lo:ListObject)=  true
        let deleteDataAndNamedRange (ws:Worksheet) = 
            ws.Names.Cast<Name>() 
            |> Seq.filter(fun e -> e.Name.StartsWith(outputrange,StringComparison.CurrentCultureIgnoreCase)) 
            |> Seq.iter  (fun (rname:Name) ->
                                System.Console.WriteLine (sprintf "%A" rname.Name)
                                try if validNamedRange rname then
                                        rname.RefersToRange.Clear() |> ignore
                                with | e -> System.Console.WriteLine (sprintf "could not delete %A content" rname.Name)
                                rname.Delete())

        let clear (rg:Range) = rg.Clear() |> ignore
        let findAndClearNamedRange (ws:Worksheet) pred = 
                      ws.Names.Cast<Name>() |> Seq.filter(fun (e:Name) -> pred(e.Name) )
                                            |> Seq.iter (fun rname -> if validNamedRange rname then rname.RefersToRange.Clear()|> ignore;
                                                                      rname.Delete())
        let findAndClearNamedTable (ws:Worksheet) pred = 
                    ws.ListObjects.Cast<ListObject>() |> Seq.filter(fun (e:ListObject) -> pred(e.Name) )
                                                      |> Seq.iter (fun rname -> if validNamedTable rname then rname.Delete()|> ignore)
        let clearOutputRanges ws  = 
            findAndClearNamedRange ws  (fun name -> name.StartsWith(ws.Name + "!"+ outputrange))

        let getFreshRange ws rangeName (roff1:int,coff1:int) (roff2:int,coff2:int) = 
            let dataTag = dataTag ws
            findAndClearNamedRange ws  (fun rname -> rname = ws.Name + "!"+ rangeName)
            let c = ws.Range(dataTag.Offset(roff1,  coff1), dataTag.Offset(roff2,  coff2))
            ws.Names.Add(rangeName, c) |> ignore
            c

        let getFreshTable  ws rangeName (roff1,coff1) (roff2,coff2) = 
            let dataTag = dataTag ws
            findAndClearNamedTable ws (fun rname -> rname = rangeName + "_"+ ws.Name) 
            let c = ws.Range(dataTag.Offset(roff1,  coff1), dataTag.Offset(roff2,  coff2))
            let t = ws.ListObjects.Add(XlListObjectSourceType.xlSrcRange,c,XlListObjectHasHeaders=XlYesNoGuess.xlYes)
            t.Name <-rangeName + "_"+ ws.Name //table name are not scoped..
            t

        let ExcelMaxStringLength = 32767 
        let truncateObj (o:obj) = match o with
                                | :? string as s -> 
                                    if s.Length > ExcelMaxStringLength
                                    then s.Substring(0,ExcelMaxStringLength) :> obj
                                    else s :> obj
                                | _ -> o

   
    let writeExcel saveCSV (ws:Excel.Worksheet) 
                   (typedCoreSchema : Declaration list)
                   (distToString: TypedDTO.Rep -> obj -> string)
                   (dbin:TypedDTO.DataBase)
                   (db:TypedDTO.DataBase) : string = 

        let wb = (ws.Parent :?> Workbook)
        let dir = if saveCSV then System.IO.Path.GetDirectoryName(wb.FullName) + @"\csv\" + System.IO.Path.GetFileNameWithoutExtension(wb.Name) + @"\" + ws.Name + @"\" else null
        let latentYellow = 0x00C0FF
        let lightGray = 0xEDEDED
        let darkGray = 0xA5A5A5
        let blue = 0xD59B5B 
        do deleteDataAndNamedRange ws
           clearOutputRanges ws
        let globalColCounter = ref 0

        TypedDTO.dbTo2DString typedCoreSchema
                    (fun tn idStrategy table staticDataBuf instanceDataBuf ->
                        let cardStatic      = Array2D.length1 staticDataBuf   - 1  //-1 for the headers
                        let size            = Array2D.length1 instanceDataBuf - 1
                        let cardInstances   = Array2D.length2 instanceDataBuf   

                        let staticDataBuf   = Array2D.map truncateObj staticDataBuf   //inefficient
                        let instanceDataBuf = Array2D.map truncateObj instanceDataBuf
                        let c = getFreshRange ws (outputrange + "_tablelabel_" + tn) (0, !globalColCounter) (0, !globalColCounter)
                        c.Value2 <- tn; c.Font.Bold <- true

                        globalColCounter := 
                            if cardStatic > 0 then
                                staticDataBuf.[0,0] <- "Name"  :> obj;  staticDataBuf.[0,1] <- "Value" :> obj
                                let rangeNameStatic = outputrange + "_static" + tn
                                let rangeStatic = getFreshRange ws rangeNameStatic (1, !globalColCounter) (1+cardStatic, !globalColCounter + 1)  //|> ignore
                                rangeStatic.Value2   <- staticDataBuf
                                let t = getFreshTable ws (rangeNameStatic   + "_table") (1, !globalColCounter) (1+cardStatic, !globalColCounter + 2 - 1)
                                let nameRange =  t.Range.selectSubRng(topcol=1)  
                                let valueRange =  t.Range.selectSubRng(topcol=2)
                                valueRange.Interior.Color <- latentYellow; t.Range.Font.Color <- 0x000000
                                valueRange.EntireColumn.AutoFit() |> ignore
                                nameRange.EntireColumn.AutoFit() |> ignore
                                (rangeStatic.Rows.[1] :?> Range).Interior.Color <- darkGray;  (rangeStatic.Rows.[1] :?> Range).Font.Color <- 0xFFFFFF            
                                !globalColCounter + 3
                            else
                                !globalColCounter 

                        let rangeNameInstance = outputrange + tn
                        let rangeInstance = getFreshRange ws rangeNameInstance              (1, !globalColCounter) (1+size,  !globalColCounter + cardInstances - 1) //|> ignore 
                        rangeInstance.Value2 <- instanceDataBuf
                        let tableInstance = getFreshTable ws (rangeNameInstance + "_table") (1, !globalColCounter) (1+size,  !globalColCounter + cardInstances - 1)

                        if saveCSV then
                            TypedDTO.write2DArrayToCSV  dir (tn^"static") staticDataBuf
                            TypedDTO.write2DArrayToCSV  dir tn            instanceDataBuf

                        let rec trColorColumns ( iInstances) (columns: Table) = 
                            match columns with
                            |  [] -> ()
                            | ((cn,({Type=ty;Markup=(_,Local,_)})) as c)::rest ->
                                 (* skip locals *)
                                 trColorColumns iInstances rest
                            | ((cn,({Type=ty;Markup=m})) as c)::rest -> 
                                 match level c with
                                 | l when l > W  -> 
                                    let col = tableInstance.Range.Columns.[1+iInstances] :?> Range
                                    match m with 
                                    | Latent _ ->
                                      col.Interior.Color <-  latentYellow
                                    | Observable _ ->
                                      col.Interior.Color <- latentYellow
                                      let nonNullIndices =  let (_,col,_,_) = dbin.[tn] in (col.[cn] :?> TypedDTO.Instance).get_NonNullIndices
                                      for i in nonNullIndices do
                                        try let ci = col.Range("A"+(i+2).ToString()) 
                                            ci.Interior.Color <- blue
                                        with _ -> assert(false)
                                    | Input _ -> 
                                      col.Interior.Color <-  blue
                                    col.EntireColumn.AutoFit() |> ignore
                                    trColorColumns ( iInstances + 1) rest 
                                 | W  -> 
                                    trColorColumns  iInstances rest
                                 | _             -> trColorColumns iInstances rest
                        let extraCol = 
                            let (_, colMap, _, _) = db.[tn]
                            match idStrategy with | FromColumn  cid ->  if colMap.ContainsKey cid then 0 
                                                                        else (rangeInstance.Columns.[1] :?> Range).Interior.Color <-blue
                                                                             1
                                                  | _ -> (rangeInstance.Columns.[1] :?> Range).Interior.Color <-blue
                                                         1
                        trColorColumns extraCol table
                        (rangeInstance.Rows.[1] :?> Range).Interior.Color <- darkGray
                        (rangeInstance.Rows.[1] :?> Range).Font.Color <- 0xFFFFFF    
                        globalColCounter := !globalColCounter + cardInstances + 1
                    )
                    distToString
                    db

        dir


     
#if FALSE

    let writeExcel2 saveCSV (ws:Excel.Worksheet) 
                   (typedCoreSchema : Declaration list)
                   (distToString: TypedDTO.Rep -> obj -> string)
                   (dbin:TypedDTO.DataBase)
                   (db:TypedDTO.DataBase) : string = 

     let wb = (ws.Parent :?> Workbook)
     let dir = if saveCSV then System.IO.Path.GetDirectoryName(wb.FullName) + @"\csv\" + System.IO.Path.GetFileNameWithoutExtension(wb.Name) + @"\" + ws.Name + @"\" else null
    

     let rec ToObj (rep:TypedDTO.Rep) (src:obj) : obj =
        (rep).Visit({ new TypedDTO.RepVisitor<obj> with
             member this.CaseIntRep                r  = match src with :? int -> src | _ -> distToString rep src :> obj
             member this.CaseArrayRep<'T>         (r:TypedDTO.ArrayRep<'T>)  = 
                                                         let ts = (src :?> IEnumerable) in  
                                                         "["+System.String.Join(",",[| for t in ts -> (ToObj (r.ElementRep) t).ToString() |])+"]" :> obj
             member this.CaseUpToRep               r  =  match src with :? int -> src | _ -> distToString rep src :> obj
             member this.CaseUpToSizeRep           r  =  match src with :? int as i -> (r.Inverse i).ToString() :> obj | _ -> distToString rep src :> obj
                                                           
             member this.CaseBoolRep               r  =  match src with :? bool -> src | _ -> distToString rep src :> obj
             member this.CaseStringRep             r  =  match src with :? string -> src | _ -> distToString rep src :> obj
             member this.CaseRealRep               r  =  match src with :? real -> src | _ -> distToString rep src :> obj
             member this.CaseGenericIComparableRep r  = src.ToString() :> obj // can this be a dist?
             member this.CaseGenericToStringRep    r  = src.ToString() :> obj // can this be dist?
             member this.CaseVectorRep r  =   match src with :?  MicrosoftResearch.Infer.Tabular.Vector -> src.ToString() :> obj | _ -> distToString rep src :> obj
             member this.CaseMatrixRep r  = match src with :?  MicrosoftResearch.Infer.Tabular.Matrix -> src.ToString() :> obj | _ -> distToString rep src :> obj
             })
     do deleteDataAndNamedRange ws
     do clearOutputRanges ws
     //let getFreshRange = getFreshRange ws 
     //let getFreshTable = getFreshTable ws 
     let id2Rep = db |> Map.map(fun k (_,_,rep,keyToPos) -> rep) // build me up incrementally
     let id2KeyToPos = db |> Map.map(fun k (_,_,_,keyToPos) -> keyToPos) // build me up incrementally
     let rec trTables globalColCounter (tables :Declaration list) = 
        match tables with
        | [] -> ()
        | (Declaration(Fun(tn),table)::tables)                    -> trTables globalColCounter tables
        | (Declaration(Table(tn, None ),table)::tables)           -> failwith "please explicit a strategy for getting the id"
        | (Declaration(Table(tn, Some idStrategy),table)::tables) ->
            let (size, colMap, idRep, keyToPos) = db.[tn]
            let cardStatic, cardInstances = 
                let rec trColumns (cardStatic, cardInstances) (columns: Table) = 
                    match columns with
                    |  [] -> cardStatic, cardInstances
                    | ((cn,({Type=ty;Markup=(_,Local,_)})) as c)::rest -> 
                       (* skip locals *)
                       trColumns (cardStatic, cardInstances) rest
                    | ((cn,({Type=ty;Markup=m})) as c)::rest -> 
                        match level c with
                        | l when l > W  -> trColumns (cardStatic, cardInstances + 1) rest
                        | W             -> trColumns (cardStatic + 1, cardInstances) rest
                        | _             -> trColumns (cardStatic, cardInstances) rest

                let extraCol = match idStrategy with | FromColumn  cid ->  if colMap.ContainsKey cid then 0 else 1
                                                     | _ -> 1
                trColumns (0, extraCol) table

            let c = getFreshRange ws (outputrange + "_tablelabel_" + tn) (0, globalColCounter) (0, globalColCounter)
            c.Value2 <- tn; c.Font.Bold <- true

            let staticDataBuf, instanceDataBuf = 
                let staticDataBuf   = Array2D.zeroCreate (1+cardStatic) 2        
                let instanceDataBuf = Array2D.zeroCreate (1+size)       cardInstances
                let rec trColumns (iStatic, iInstances) (columns: Table) = 
                    match columns with
                    |  [] -> staticDataBuf, instanceDataBuf
                    | ((cn,({Type=ty;Markup=(_,Local,_)})) as c)::rest ->
                       (* skip locals *)
                       trColumns (iStatic, iInstances) rest
                    | ((cn,({Type=ty;Markup=m})) as c)::rest ->
                        let rep = TypedDTO.rep (id2Rep,id2KeyToPos) ty
                        let ToObj = ToObj rep 
                        match level c with
                        | l when l > W  -> 
                            instanceDataBuf.[0,iInstances] <- cn :> obj
                            let cv = (colMap.[cn] :?> TypedDTO.Instance).get_NonNullValues
                            for i in  0 .. size - 1  do instanceDataBuf.[1+i,iInstances] <- truncateObj (ToObj (cv.GetValue i))
                            trColumns (iStatic, iInstances + 1) rest 
                        | W  -> 
                            staticDataBuf.[1+iStatic, 0]   <-  cn :> obj
                            staticDataBuf.[1+iStatic, 1] <- truncateObj (ToObj ((colMap.[cn] :?> TypedDTO.Static).Value()))
                            trColumns (iStatic + 1, iInstances) rest
                        | _             -> trColumns (iStatic, iInstances) rest

                let extraCol = 
                    match idStrategy with | FromColumn  cid ->  if colMap.ContainsKey cid then 0 
                                                                else let posToKey = keyToPos |> Seq.map (fun kv -> kv.Value, kv.Key) |> Map.ofSeq
                                                                     instanceDataBuf.[0, 0]  <- cid :> obj
                                                                     for i in  0 .. size - 1  do instanceDataBuf.[1+i, 0] <- (posToKey.[i]) :> obj //TBR
                                                                     1
                                          | _ -> instanceDataBuf.[0,0] <- "ID" :> obj;
                                                 for i in  0 .. size - 1  do instanceDataBuf.[1+i, 0] <- i :> obj
                                                 1
                trColumns (0, extraCol) table

            (* colours *)
            let latentYellow = 0x00C0FF
            let lightGray = 0xEDEDED
            let darkGray = 0xA5A5A5
            let blue = 0xD59B5B 

            let globalColCounter = 
                if cardStatic > 0 then
                    staticDataBuf.[0,0] <- "Name"  :> obj;  staticDataBuf.[0,1] <- "Value" :> obj
                    let rangeNameStatic = outputrange + "_static" + tn
                    let rangeStatic = getFreshRange ws rangeNameStatic (1, globalColCounter) (1+cardStatic, globalColCounter + 1)  //|> ignore
                    rangeStatic.Value2   <- staticDataBuf
                    let t = getFreshTable ws (rangeNameStatic   + "_table") (1, globalColCounter) (1+cardStatic, globalColCounter + 2 - 1)
                    let nameRange =  t.Range.selectSubRng(topcol=1)  
                    let valueRange =  t.Range.selectSubRng(topcol=2)
                    valueRange.Interior.Color <- latentYellow; t.Range.Font.Color <- 0x000000
                    valueRange.EntireColumn.AutoFit() |> ignore
                    nameRange.EntireColumn.AutoFit() |> ignore
                    (rangeStatic.Rows.[1] :?> Range).Interior.Color <- darkGray;  (rangeStatic.Rows.[1] :?> Range).Font.Color <- 0xFFFFFF            
                    globalColCounter + 3
                else
                    globalColCounter 
            
            let rangeNameInstance = outputrange + tn
            let rangeInstance = getFreshRange ws rangeNameInstance              (1,  globalColCounter) (1+size,  globalColCounter + cardInstances - 1) //|> ignore 
            rangeInstance.Value2 <- instanceDataBuf

            let tableInstance = getFreshTable ws (rangeNameInstance + "_table") (1,  globalColCounter) (1+size,  globalColCounter + cardInstances - 1)

            if saveCSV then
                writeCSV dir (tn^"static") staticDataBuf
                writeCSV dir tn instanceDataBuf
       
            
            let rec trColorColumns ( iInstances) (columns: Table) = 
                match columns with
                |  [] -> ()
                | ((cn,({Type=ty;Markup=(_,Local,_)})) as c)::rest ->
                     (* skip locals *)
                     trColorColumns iInstances rest
                | ((cn,({Type=ty;Markup=m})) as c)::rest -> 
                     match level c with
                     | l when l > W  -> 
                        let col = tableInstance.Range.Columns.[1+iInstances] :?> Range
                        match m with 
                        | Latent _ ->
                          col.Interior.Color <-  latentYellow
                        | Observable _ ->
                          col.Interior.Color <- latentYellow
                          let nonNullIndices =  let (_,col,_,_) = dbin.[tn] in (col.[cn] :?> TypedDTO.Instance).get_NonNullIndices
                          for i in nonNullIndices do
                            try let ci = col.Range("A"+(i+2).ToString()) 
                                ci.Interior.Color <- blue
                            with _ -> assert(false)
                        | Input _ -> 
                          col.Interior.Color <-  blue
                        col.EntireColumn.AutoFit() |> ignore
                        trColorColumns ( iInstances + 1) rest 
                     | W  -> 
                        trColorColumns  iInstances rest
                     | _             -> trColorColumns iInstances rest
            let extraCol = 
                match idStrategy with | FromColumn  cid ->  if colMap.ContainsKey cid then 0 
                                                            else (rangeInstance.Columns.[1] :?> Range).Interior.Color <-blue
                                                                 1
                                       | _ -> (rangeInstance.Columns.[1] :?> Range).Interior.Color <-blue
                                              1
            trColorColumns extraCol table
            (rangeInstance.Rows.[1] :?> Range).Interior.Color <- darkGray
            (rangeInstance.Rows.[1] :?> Range).Font.Color <- 0xFFFFFF    
        
            
            

            trTables  (globalColCounter + cardInstances + 1) tables
     trTables 0 typedCoreSchema
     dir
#endif