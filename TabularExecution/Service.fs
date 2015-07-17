namespace MicrosoftResearch.Infer.Tabular


[<AutoOpen>]
module Service = 
    open System
    open System.Threading;
    open System.IO;
    open MicrosoftResearch.Infer.Tabular.DataLayer
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Json
    open MicrosoftResearch.Infer
    open MicrosoftResearch.Infer.Utils
    open MicrosoftResearch.Infer.Distributions
    open MicrosoftResearch.Infer.Collections
    open System.Collections.Generic
    open Syntax
    open NewIO

    let hasDuplicate xs =
      let _, duplicate = 
         Seq.fold(fun ((unique, duplicate))  e ->  if Set.contains e unique
                                                   then (unique, Set.add e duplicate)
                                                   else (Set.add e  unique, duplicate))
                  (Set.empty, Set.empty)
                  xs
      duplicate.Count <> 0



    let readTable2 (loader:ILoader)(schema:Schema) (validateMode:bool)= 
        let keysTypes = Dictionary<string, Type>() //mutable type

        try
          let ((tablesNormalizedtablesRaw),rest) = 
                  schema 
                  |> Schema.OnlyTables |> Schema.GetNameListTS //Since we need to map foreign keys to the position in the arrays
                                                               //we need to load in topological order to always have the mapping
                  |> List.scan(fun (_, (tableIdStrategy:Map<TableName,IdStrategy>, tableLogs:Map<TableName,Log>, tableIdToPos:Map<TableName, Map<IComparable, int>>)) tableName ->
                              let ((Declaration(Table(_, oId), cols)) as table) = tableName |> (Schema.GetTableByName schema)
                              let cols = if validateMode then Table.OnlyLinks cols else Table.OnlyConcreteDataColumns cols 

                              let (idStrategy,idSelectionLog1) = 
                                    if oId.IsSome then oId.Value, []
                                    else loader.InferIDStrategy tableName None
                              let (colNametoHPos, res) = loader.ReadConcrete tableName idStrategy (cols |> List.map fst)
                              let ids, data = res |> Seq.unzip

                              do if not (ids |> Seq.isEmpty) then
                                    keysTypes.Add(tableName, ids |> Seq.head |> (fun o -> o.GetType()))

                              let idSelectionLog = 
                                 if validateMode && hasDuplicate ids 
                                 then (Log.Warning(sprintf "duplicate ids founds in table :%A" tableName))::idSelectionLog1
                                 else idSelectionLog1

                              let thisTableIdToPos = 
                                 try ids |> Seq.mapi rtuple |> Map.ofSeq
                                 with | e -> let msg = sprintf "The type of id for table %A were found to be inhomogenous" tableName
                                             failwith (msg + "\n error :" + e.Message)

                              let idDataLog, featurized, original = 
                                  let idDataLog = ref List.empty<Log.LogValue>
                                  let d = data |> Seq.toArray 

                                  do cols |> Seq.iter (fun (colname, _) -> if not (colNametoHPos.ContainsKey colname) 
                                                                           then failwith (sprintf "could not find the model column %A in the data columns \n %A" colname (colNametoHPos.Keys |> Seq.toArray) ))

                                  let d2 = Array.map Array.copy d
                                  //destructive remapping
                                  try
                                     do d |> Array.iter(fun el -> 
                                                         cols |> Seq.iter(fun (colname, col) ->   
                                                                          try 
                                                                           //if this is a link, we try to find the element whose key is the value of the column
                                                                           let a = match col.Type with
                                                                                    | T_Upto(SizeOf table)
                                                                                    | T_Upto(TypedExp(SizeOf table,_))
                                                                                    | T_Link table  -> 
                                                                                       try  let allowNull = match col.Markup with Observable _ -> true | _ -> false
                                                                                            let v = el.[colNametoHPos.[colname]]
                                                                                            match v with
                                                                                             | null -> if allowNull 
                                                                                                       then null 
                                                                                                       else failwith (sprintf "unexpected null in table %A, column %A" table colname)
                                                                                             | _ ->
                                                                                             box tableIdToPos.[table].[System.Convert.ChangeType(v,keysTypes.[table]) :?> IComparable ] 
                                                                                       with |e -> e.Data.Add("table", table); raise e
                                                                                    | _     -> el.[colNametoHPos.[colname]]
                                                                           try el.[colNametoHPos.[colname]] <- a //destructive remapping
                                                                           with |e -> failwith (sprintf "cannot assign object column :%A, row : %A, error :%A" colname el e.Message)
                                                                          with |e -> let msg = sprintf "While reading table %A, the following row was encoutered\n%A\n" tableName el  +
                                                                                               sprintf "its column named %A in position %A " colname colNametoHPos.[colname] +
                                                                                               sprintf "references the element of id %A from table %A\n"  el.[colNametoHPos.[colname]] e.Data.["table"] + 
                                                                                               sprintf "this id can not be found"
                                                                                     if validateMode && List.length !idDataLog < 10 then
                                                                                       idDataLog := (Log.Warning(sprintf "%A \n error :%A" msg  e.Message))::!idDataLog
                                                                                     else
                                                                                       failwith (sprintf "%A \n error :%A" msg  e.Message)))
                                  with | e ->  if validateMode then () else raise e 
                                  !idDataLog, d |> Array.toSeq , d2 |> Array.toSeq
                              ((tableName, (colNametoHPos, featurized)),(tableName, (colNametoHPos, original))), //not used in iteration
                              (tableIdStrategy.Add(tableName,idStrategy), tableLogs.Add(tableName,idSelectionLog@idDataLog), tableIdToPos.Add(tableName, thisTableIdToPos)))
                              ((("", (Map.empty, Seq.empty)),("", (Map.empty, Seq.empty))), (Map.empty, Map.empty, Map.empty))
                              |> List.unzip

          let tablesNormalized, tablesRaw = tablesNormalizedtablesRaw |> List.unzip
          let dictData, dicDataRaw, (dicIdStrategy, dicLog, dicMappings) = 
             tablesNormalized |> Seq.filter (fun (n,_) -> n <> "") |> Map.ofSeq,
             tablesRaw |> Seq.filter (fun (n,_) -> n <> "") |> Map.ofSeq,
             rest |> List.rev |> (fun ls ->  if ls.IsEmpty 
                                             then Map.empty, Map.empty, Map.empty 
                                             else let (lastDicIdStrategy, lastDicLog, lastDicIdToPos) = (List.head ls) 
                                                  lastDicIdStrategy, lastDicLog, lastDicIdToPos)

          let schemaWithIdStrategy = 
            let fillIDStrategy (dicIdStrategy:Map<TableName,IdStrategy>) schema = 
                let rec trTables schema' = function 
                    | [] -> schema' |> List.rev
                    | (Declaration(Fun(tn),table) as e::tables)    ->   trTables (e::schema') tables
                    | (Declaration(Table(tn, None),table)::tables)  ->  trTables ((Declaration(Table(tn,Some(dicIdStrategy.[tn])),table))::schema') tables
                    | (Declaration(Table(tn, Some _),table) as e::tables) -> trTables (e::schema') tables
                trTables [] schema            
            fillIDStrategy dicIdStrategy schema

          keysTypes, dicLog, dicIdStrategy, dictData, schemaWithIdStrategy , (DTOToTypedDTO.readFromDTO schemaWithIdStrategy (DTO dicDataRaw)), dicMappings 
        with | e -> failwith (sprintf "%A \nmake sure the model and the database are conformant" e.Message)


    let readTable    (loader:ILoader)(schema:Schema) =  readTable2 loader schema false
    let validateLinks (loader:ILoader)(schema:Schema) = readTable2 loader schema true

#if DEAD
    module FArray = Microsoft.FSharp.Collections.Array
    type Microsoft.FSharp.Collections.Map<'K, 'V when 'K : comparison > with 
        member t.Keys = t |> Seq.map(fun e -> e.Key)

    //produce a total enumeration of a object made up of recursive enumerations
    let expand (o:obj) = 
      let rec expandInner o k = seq {
 //crusso: disabled the then branch to turn of flattening 
         if false && typeof<System.Collections.IEnumerable>.IsAssignableFrom(o.GetType()) then
            //we start a new counter for this level
            let i = ref 0
            let ao = box o :?> System.Collections.IEnumerable
            for a in ao do
               //we will append to the name an extra parameter corresponding to this level
               yield! expandInner a  (fun (n,final) -> k(n+"_"+ (!i).ToString(),final))
               i := !i + 1 
         else
       
               //object need no prepend
               yield k("", o)
         }
      expandInner o id
    
    let expandNames (colNames: Map<ColumnName,int>, posterior: obj array) = 
      (colNames.Keys  |> Seq.toList )
        |> Seq.unfold (function | [] -> None 
                                | (colName)::xs ->
                                    Some (expand (posterior.[colNames.[colName]]) |> Seq.map (fun (n,o) -> colName+n,o),xs))
        |> Seq.concat


    let (|MatchOne|_|) xs cond = xs |> List.tryFind (cond)
      

    let mapLinkPosToId(posToId:Map<int,IComparable>) (d:Discrete) = 
      let mode = d.GetMode()
      if d.GetLogProb(mode) = 0. then
            Distributions.Discrete.PointMass(coerce posToId.[mode], d.Dimension) //dimension is not enough when having non contiguous range of id
      else
            //for each index, create the sparse vector with postoId as element
            Distributions.Discrete(Maths.SparseVector.FromSparseValues(d.Dimension, 0.,
                                                                       System.Collections.Generic.List<_>(
                                                                        d.GetProbs() |> Seq.mapi(fun i lp -> ValueAtIndex(coerce posToId.[i],lp)) 
                                                                       )))

    let tranformLinkToUserIds (posToId:Map<TableName,Map<int,IComparable>>, schema:Schema, (DistDTO data): DistDTO) : DistDTO = 
      //for each table in the data, we find the definition
      data |> Map.map (fun dk ((cnmapi, saobj) as dv) -> 
                           match schema |> List.tryFind (fun e -> match e with | Declaration(Table(tn,_),table) as e when tn = dk -> true |_ -> false) with
                           | Some(Declaration(Table(tn,_),table)) -> 
                              //for each column in the table data, we find the definition 
                              cnmapi |> Map.iter (fun dcn i -> match table |> List.tryFind (fun (scn, sct) -> scn = dcn) with
                                                                  | Some(scn, sct) ->  match sct.Type with 
                                                                                       | T_Link(othertable) -> let transform (o:obj) = 
                                                                                                                  if o.GetType() = typeof<int> then
                                                                                                                     o 
                                                                                                                  else
                                                                                                                     try
                                                                                                                        let tyo = o :?> Discrete
                                                                                                                        mapLinkPosToId posToId.[othertable] tyo |> box
                                                                                                                     with
                                                                                                                        |e -> o // failwith "error in transforming link internal position to ids from schema"
                                                                                                               //we mutate the obj
                                                                                                               saobj |> Seq.iter(fun aobj -> aobj.[i] <- box (transform(aobj.[i])))
                                                                                       | _ -> ()
                                                                     | None -> ())
                              dv
                           | _ -> dv
         )
      |> DistDTO      
      

    let saveStichedYZ(storer:IStorer, (schema:Schema, dicIdStrategy:Map<TableName,IdStrategy>, idToPos:Map<TableName,Map<IComparable,int>>
                                    , (dtoIn:DTO, predictedPZ:DistDTO, ((KnowDTO mDW):KnowDTO)))
                                    , saveInput) = 
          let posToID = idToPos |> Map.map (fun tname lIdToPos -> lIdToPos |> Seq.map (fun kv -> kv.Value, kv.Key) |> Map.ofSeq)
          let tables =  schema  |> List.choose(function | Declaration(Table(n,_),t)  -> Some (n,  t) | _ -> None)
                                 
          let predictedPZ = tranformLinkToUserIds(posToID, schema, predictedPZ)           
          let merged = (predictedPZ.AsString(posToID,tables)).merge(dtoIn) //we add the predictions of the different tables (PZ) to the input of the different tables (dtoIn)


          tables |> List.iter(fun (tableName, columns) -> 
                                 let colDef = columns |>  List.choose (fun (cn, col) -> match col.Type, col.Markup with | T_Det(bt,_), _ -> Some (cn,bt) | _ -> None)
                                 storer.CreateResultTable(tableName ,saveInput,(dicIdStrategy.[tableName]),posToID ,merged.[tableName], predictedPZ.[tableName], dtoIn)) 


    let savePosterior(storer:IStorer, (schema:Schema, idToPos:Map<TableName,Map<IComparable,int>>
                                    , (dtoIn:DTO, predictedPZ:DistDTO, ((KnowDTO mDW):KnowDTO)))
                                    , stackPosterior) =   
          if not (mDW  |> Seq.forall(fun kv -> (fst kv.Value).IsEmpty ) ) then
            let fstring = storer.GetTimeStampedTable (not stackPosterior)
            mDW |> Map.iter(fun tableName (colNames, posterior) -> 
                                     let postTName = "posterior_" + tableName
                                     let expanded = expandNames (colNames, posterior)

                                     let save () = let posteriorNames  = expanded |> Seq.map (fun (n,_) -> n, B_String)       |> Seq.toList
                                                   let posteriors      = expanded |> Seq.map (snd >> (**) print List.empty >> box) |> Seq.toArray
                                                   let posNamesToPos = Seq.zip (expanded |> Seq.map fst) integers |> Map.ofSeq
                                                   fstring postTName  posteriorNames posNamesToPos posteriors
                                     try save ()
                                     with | e ->   storer.DropTable(postTName)
                                                   save())

#endif
