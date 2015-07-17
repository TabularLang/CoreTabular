namespace MicrosoftResearch.Infer.Tabular.DataLayer

open MicrosoftResearch.Infer.Tabular //fix this module namespace 
open QuickGraph;
open System.Diagnostics;
open System.Collections.Generic
open System.Collections.Specialized
open System.Collections.ObjectModel
open System.ComponentModel
open System

module NewIO =
  open MicrosoftResearch.Infer.Tabular.Syntax
  open MicrosoftResearch.Infer.Tabular.StringToInferNet
  open Log

  type dataIOOut = Map<ColumnName,int> * (IComparable * obj array) seq
  type dataIOJoin= Map<ColumnName,int> * (obj array) seq               //does not carry id
  type dataIOIn  = Map<ColumnName,int> * (int option * obj array) seq  //some storage provide id automatically
  type dataDefinition = (ColumnName * BaseType) list 


  type Log = LogValue list

  type ILoader = 
      inherit System.IDisposable 
      abstract member InferIDStrategy : TableName -> ID option -> IdStrategy  * Log
      abstract member GetSchema       : unit                    -> Schema
      //abstract member ReadEntities : List<TableName * Table> -> System.Collections.Generic.IDictionary<TableName, ColumnName array * obj array seq>
      abstract member ReadConcrete    : TableName -> IdStrategy -> ColumnName list -> dataIOOut 

  type IStorer  = 
      inherit System.IDisposable 
      //abstract member TableNames                        : string seq 
      abstract member TableExists                    : TableName ->  bool
      abstract member DropTable                      : TableName ->  unit
      abstract member CreateTable                    : TableName -> dataDefinition -> unit
      abstract member StoreTable                     : TableName ->  dataIOIn -> unit
      /// expandee -> save input -> idStrategy -> fullPostoIdMapping for foreign position to foreign Id  -> the data itself -> the distributions for smarter printing
      abstract member CreateResultTable             : TableName * bool * IdStrategy *  Map<TableName,Map<int,IComparable>> * dataIOJoin  * dataIOJoin * DTO -> unit
      /// reuse former posterior table -> current table name -> posteriorNames*B_String -> colNameToPos -> posteriors
      abstract member GetTimeStampedTable  : bool (*reuse*) -> (TableName -> dataDefinition -> ((Map<ColumnName,int> -> obj array -> unit)))

 
///A light module for dealing with relational data repository, based on QuickGraph
[<AutoOpen>]  
module DBSchema  = 
    open QuickGraph;
    open System.Diagnostics;
    open System.Collections.Generic
    open System.Collections.Specialized
    open System.Collections.ObjectModel
    open System.ComponentModel
    open System

    type DBColumn = | Local  of Type * string
                    | Remote of string(*othertable*) * string

    type DBLabel  = string
    type DBTable = { fields:IDictionary<DBLabel,DBColumn> }

    module Seq =
        let unzip seq =
            let seq = Seq.cache seq
            Seq.map fst seq, Seq.map snd seq


    ///ForeignName = name of the column in Foreign table holding the information whose name is Name in the To table
    type DBJoin  = { ForeignName : string;  Name : string}
    
    ///toTable = foreignTable, the ids of primary table 'From' are used in foreign table 'To'
    type DBRelation = 
        inherit Edge<string * DBTable>
        val join : DBJoin                                   // need access to DBJoin
        new (fromTable, toTable, j : DBJoin  ) = 
            {inherit Edge<string * DBTable>(fromTable, toTable);
            join = j}
    type DBEntityGraph ()                                = inherit BidirectionalGraph<string * DBTable, DBRelation>()

    type IdToRef   = string * string // foreign key
    type IdFromRef = string * string // primary key

    let ToDBEntityGraph(relations:((IdToRef*IdFromRef) seq), tables:(string*((string*Type) list)) seq) =
            let g = new DBEntityGraph()
            let lookupR = new Dictionary<_,_>()
            for (idToRef, idFromRef) in relations do //we will want to invert the dependency to track
              lookupR.Add(idToRef, idFromRef)

            let lookupD = new Dictionary<_,_>()
            for (tName, tFields) in tables  do 
              let fields = Seq.zip (Seq.map fst tFields)
                                   ([ for (fName,fTyp) in tFields ->
                                      if lookupR.ContainsKey(tName, fName)   //this is a foreign field
                                      then Remote((*fst*) lookupR.[(tName, fName)]) 
                                      else DBColumn.Local (fTyp, fName)])  |> dict

              let v = tName, { fields = fields}
              lookupD.Add(v)
              g.AddVertex(v) |> ignore
            for kv in lookupR do
                let r = new DBRelation((fst kv.Value, lookupD.[fst kv.Value]), (fst kv.Key, lookupD.[fst kv.Key]), {ForeignName =  snd kv.Value; Name = snd kv.Key} )
                g.AddEdge(r) |> ignore
            g 


    open QuickGraph;
    open System.Collections.Generic
    open MicrosoftResearch.Infer.Tabular.Syntax

        
    let inferPKind (getUniqueValues: string -> Set<IComparable>) typ fieldName noEmptyValue = 
      
               
        let Mdummy = MExp (Var "to be completed")
        let inputOrModel ty m = if noEmptyValue() then ty, Input else makeDet ty R, Observable(m)
        let getCat () = 
            let set = getUniqueValues fieldName
            try if System.Convert.ToInt32(set.MinimumElement) >= 0 
                then 
                 try let max = System.Convert.ToInt32(set.MaximumElement)
                     let card = Const(IntConst (max+1))
                     inputOrModel  (T_Upto (card)) (MCall("CDiscrete", ["N", card]))
                 with _ -> inputOrModel T_String Mdummy
                else inputOrModel T_Int Mdummy
            with _ -> inputOrModel T_Int Mdummy
                   

        match typ with       
        |  x when x = typeof<bigint>                                  ->  inputOrModel T_Int Mdummy
        |  x when x = typeof<bool>                                    ->  inputOrModel T_Bool (MCall("CBernoulli", []))
      //|  x when x = typeof<Binary>                                  ->  
      //|  x when x = typeof<byte>                                    ->  
      //|  x when x = typeof<char>                                    ->  
      //| dbCurrency  typeof<Currency>                                ->  
        |  x when x = typeof<System.DateTime>                         ->  inputOrModel T_String Mdummy
      //|  x when x = typeof<decimal>                                 ->  
        |  x when x = typeof<double>                                  ->  inputOrModel T_Real (MCall("CGaussian", []))
        |  x when x = typeof<float>                                   ->  inputOrModel T_Real (MCall("CGaussian", []))
        |  x when x = typeof<single>                                  ->  inputOrModel T_Real (MCall("CGaussian", []))
       
        |  x when x = typeof<int>                                     ->  getCat ()
        |  x when x = typeof<int16>                                   ->  getCat ()
        |  x when x = typeof<int64>                                   ->  getCat ()
        |  x when x = typeof<System.Guid>                             ->  inputOrModel T_String Mdummy
      //|  x when x = typeof<Long Binary (OLE Object)>                ->  
        |  x when x = typeof<string>                                  ->  inputOrModel T_String Mdummy
      //|  x when x = typeof<Time>                                    ->  
      //|  x when x = typeof<Time Stamp>                              ->  
      //|  x when x = typeof<VarBinary>                               ->  
        | _ -> inputOrModel T_String Mdummy


    type DBTable with
                    member x.GetNewTable(id : string option, getUniqueValues : ColumnName  -> Set<IComparable>, fieldHasNoEmptyValue) = 
                        let GetNewColumn(x,getUniqueValues, noEmptyValue) = 
                           let ttype, markup = match x with  
                                               | Remote(othertable,_)  when noEmptyValue ()           -> T_Link(othertable), Input (*referential integrity*)
                                               | Remote(othertable,_)                                 -> makeDet (T_Link(othertable)) R, Observable(MCall("CDiscrete", ["N", SizeOf(othertable)]))
                                               | DBColumn.Local(typ, name)                            -> inferPKind getUniqueValues typ name noEmptyValue
                           let ttype = match markup with 
                                       | Input -> ttype
                                       | _ -> makeDet ttype R
                           {Type = ttype; Markup=markup}


                        x.fields   |> Seq.where (fun kv -> Some kv.Key <> id)
                                   |> Seq.map (fun kv -> kv.Key, GetNewColumn(kv.Value, getUniqueValues, fieldHasNoEmptyValue kv.Key)) 
                                   //|> Seq.filter (fun (k,v) -> v.Type <> T_String )
                                   |> Seq.toList

    type IEnumerable<'T> with member x.CastSeq<'T> () = x |> Seq.cast<'T>

    ///toTable = foreignTable, the ids of primary table 'From' are used in foreign table 'To'
//    type DBRelation(fromTable, toTable, join : DBJoin  ) = inherit Edge<string * DBTable>(fromTable, toTable) 

    /// given a DBGraph, return a list of the tables in topological order
    /// the first has no dependencies, the second may only depend on the first, etc.
    let getTablesTopological (dbgraph:DBEntityGraph) =
        /// recursive helper function
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
                    tablesRemain.[nextTableIdx] :: sortTablesTopo (Array.append tablesRemain.[..(nextTableIdx-1)] tablesRemain.[(nextTableIdx+1)..] )
                    
        dbgraph.Vertices.CastSeq() |> Array.ofSeq |> sortTablesTopo

