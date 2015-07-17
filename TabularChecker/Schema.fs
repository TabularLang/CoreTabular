module MicrosoftResearch.Infer.Tabular.Schema

open Syntax
open Types
open Checker
open Model
open Table
open System.Collections.Generic
type Log = Map<TableName,Table.Log>

module Tabular = Syntax

let rec synthSchema (g:Env) (s:Schema) : (Log * Error * (Declaration list * SchemaType)) =
    match s with
      [] -> (Map.empty, false, ([], (ERT, ERT, ERT, ERT, ERT)))
    | (Declaration(Table (tName,oStratId),tb)) :: tl ->
        let (tlog,err,tb') = synthTable true g tb
        let (tb1, t) = tb'
        let (ht, wt, xt, yt, zt) = t
        let g1 = envInsertTable g tName t
        let (slog',err',s1') = synthSchema  g1 tl
        let slog = slog'.Add(tName,tlog)
        let (s2, t1) = s1'
        let (h, w, x, y, z) = t1
        let h1 =  ( (tName, T_Record ht) :: h)
        let w1 =  ( (tName, T_Record wt) :: w)
        let x1 =  ( (tName, T_Record xt) :: x)
        let y1 =  ( (tName, T_Record yt) :: y)
        let z1 =  ( (tName, T_Record zt) :: z)
        (slog,err||err',(((Declaration(Table(tName, oStratId),tb1) :: s2), (h1, w1, x1, y1, z1))))
    | (Declaration(Fun tName,tb)) :: tl ->
        let (tlog,err,tb') = synthTable false g tb
        let (tb1, t) = tb'
        let (ht, wt, xt, yt, zt) = t
        let (tlog,err) = 
          match yt with 
          | [(cn,e)] -> 
            if cn = tName || cn = "ret" then (tlog,err)
            else (tlog.Add(cn,Table.Err (sprintf "function %O has observable column named %O, should be named %O" tName cn tName)), true)
          | _ -> 
            (tlog.Add(tName,Table.Err (sprintf "function %O has zero or several output columns - only one expected" tName)), true) 
        let rec checkRet cols =
                match cols with
                |  [_,{Type=_; Markup=Observable _}] -> (tlog,err)
                |  [_] | [] -> (tlog.Add(tName,Table.Err (sprintf "function %O must end in an output column named 'ret' or %O" tName tName)), true)
                | _ ::cols -> checkRet cols
        let (tlog,err) = checkRet tb
        let defaults = List.foldBack (fun col defaults -> 
                                          match col with 
                                          | (cn,{Type=_;Markup=Hyper e}) -> (cn,e)::defaults
                                          | _ -> defaults) tb []
        let g1 = if not err 
                 then envInsertModel g tName (t,defaults,Y) 
                 else g // what binding time should we use?
        let (slog',err',s1') = synthSchema  g1 tl
        let slog = slog'.Add(tName,tlog)
        let (s2, t1) = s1'
        let (h, w, x, y, z) = t1
        let h1 =  ( (tName, T_Record ht) :: h)
        let w1 =  ( (tName, T_Record wt) :: w)
        let x1 =  ( (tName, T_Record xt) :: x)
        let y1 =  ( (tName, T_Record yt) :: y)
        let z1 =  ( (tName, T_Record zt) :: z)
        (slog,err||err',(((Declaration(Fun tName, tb1) :: s2), (h1, w1, x1, y1, z1))))

let typeSchema schema =  
    synthSchema Types.G_Empty (Library.prelude@schema )

let checkSchema schema = 
    let (log,err,(typedFullSchema,_)) = typeSchema schema
    let errors = Map.fold (fun s tb log -> 
                                Map.fold (fun s col v -> match v with 
                                                        | (Table.Err msg) -> s+(sprintf "\nTable %A, column %A:\n %A" tb col msg) 
                                                        | _ -> s) s log) "" log
    if err then failwithf "type-checking error: %s" errors

(*
let logToStr log = Map.fold (fun s tb log -> Map.fold (fun s col v -> 
                                                     match v with 
                                                     |  (Table.Err msg) ->
                                                      s+(sprintf "\n %A %A : %A" tb col msg)
                                                     | _ -> s) 
                                                     s log)
  
                             "" log 
                             
                               
let elaborateSchema schema =
      let (log,err,(typedSchema,_)) as ret = typeSchema schema
      if err then 
        ret
      else 
      let coreSchema = coreS (Library.prelude @ schema) 
      synthSchema Types.G_Empty coreSchema
  *) 
(*
and tableAsMap (tb':TableTyped<MarkupTyped>) : Map<string,string>=
    let (tb, _) = tb'
    let columns = tb.Columns
    List.fold (fun m -> fun column' ->
                 let (name, col') = column'
                 match col'.Markup with
                   Input -> m
                 | Latent n -> let (_,t) = n in m.Add(name, modelTypeToStr t)
                 | Observable n -> let (_,t) = n in m.Add(name, modelTypeToStr t))
                     Map.empty columns

and columnTypesAsMap (db':DatabaseT) : (Map<string,Map<string,string>>) = 
    let (db, _) = db'
    let tables' = db.Tables
    List.fold (fun m -> fun table' ->
                          let (name, tb') = table'
                          m.Add(name, tableAsMap tb') )
                            Map.empty tables'
*)