namespace MicrosoftResearch.Infer.Tabular.DataLayer

open QuickGraph;
open System.Diagnostics;
open System.Collections.Generic
open System.Collections.Specialized
open System.Collections.ObjectModel
open System.ComponentModel
open System


module NewIO =
  open MicrosoftResearch.Infer.Tabular.Syntax

  type dataIOOut = Map<ColumnName,int> * (IComparable * obj array) seq
  type dataIOJoin= Map<ColumnName,int> * (obj array) seq               //does not carry id
  type dataIOIn  = Map<ColumnName,int> * (int option * obj array) seq  //some storage provide id automatically
  type dataDefinition = (ColumnName * BaseType) list 


  type ILoader = 
      inherit System.IDisposable 
      abstract member GetSchema    : unit                    -> Schema
      //abstract member ReadEntities : List<TableName * Table> -> System.Collections.Generic.IDictionary<TableName, ColumnName array * obj array seq>
      abstract member ReadConcrete : TableName -> ColumnName list -> dataIOOut


  type IStorer  = 
      inherit System.IDisposable 
      //abstract member TableNames                        : string seq 
      abstract member TableExists                    : TableName ->  bool
      abstract member DropTable                      : TableName ->  unit
      abstract member CreateTable                    : TableName -> dataDefinition -> unit
      abstract member StoreTable                     : TableName ->  dataIOIn -> unit
      /// expandee -> expander -> colname for foreign key in expander -> what to write -> where to find it -> unit)
      abstract member CreateExpansionTable           : TableName (*basedon*) -> bool -> Map<TableName,Map<int,IComparable>> -> (dataIOJoin -> unit)
      abstract member GetTimeStampedTable  : bool (*reuse*) -> (TableName -> dataDefinition -> ((Map<ColumnName,int> -> obj array -> unit)))


 
[<AutoOpen>]  //DB level structures, which are able to be transformed into PStructures
module NewDBSchema  = 
    open QuickGraph;
    open System.Collections.Generic
    open MicrosoftResearch.Infer.Tabular.Syntax

        
    let inferPKind (getCard: string -> int) typ fieldName = 
        let getCat fieldname =  let card = getCard fieldName in T_Int, Input // Observable(MCall("CDiscrete", ["dimension", Const(IntConst card)]))
        match typ with       
        |  x when x = typeof<bigint>                                  ->  getCat  fieldName 
        |  x when x = typeof<bool>                                    ->  T_Bool, Input //Observable(MCall("CBernoulli", []))
      //|  x when x = typeof<Binary>                                  ->  
      //|  x when x = typeof<byte>                                    ->  
      //|  x when x = typeof<char>                                    ->  
      //| dbCurrency  typeof<Currency>                                ->  
        |  x when x = typeof<System.DateTime>                         ->  getCat fieldName 
      //|  x when x = typeof<decimal>                                 ->  
        |  x when x = typeof<double>                                  ->  T_Real, Input
        |  x when x = typeof<float>                                   ->  T_Real, Input
        |  x when x = typeof<System.Guid>                             ->  getCat  fieldName 
        |  x when x = typeof<int>                                     ->  getCat  fieldName 
        |  x when x = typeof<int16>                                   ->  getCat  fieldName 
        |  x when x = typeof<int64>                                   ->  getCat  fieldName 
      //|  x when x = typeof<Long Binary (OLE Object)>                ->  
        |  x when x = typeof<string>                                    ->  T_String, Input
      //|  x when x = typeof<Numeric>                                 ->  
        |  x when x = typeof<single>                                  ->  getCat  fieldName  
        |  x when x = typeof<string>                                  ->  T_String, Input
      //|  x when x = typeof<Time>                                    ->  
      //|  x when x = typeof<Time Stamp>                              ->  
      //|  x when x = typeof<VarBinary>                               ->  
        | _ -> failwith "not supported"

    type DBColumn with 
                   member x.GetNewColumn(getFieldCardinality) = 
                        let ttype, markup = match x with  
                                            | Remote(othertable) -> T_Link(othertable), Input //Observable(MCall("CDiscrete", ["dimension", SizeOf(othertable)]))
                                            | Local(typ, name)   -> inferPKind getFieldCardinality typ name
                        {Type = ttype; Markup=markup}
    type DBTable with
                    member x.GetNewTable(getfieldCardinality : ColumnName  -> int) = 
                        x.fields   |> Seq.map (fun kv -> kv.Key, kv.Value.GetNewColumn(getfieldCardinality)) 
                                   //|> Seq.filter (fun (k,v) -> v.Type <> T_String )
                                   |> Seq.toList


