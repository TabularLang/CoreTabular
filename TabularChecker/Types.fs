module MicrosoftResearch.Infer.Tabular.Types
open Syntax

//type ExprType = ColumnType

type Ident = string


// check implicit ordering is as expected
assert(D < R) 
assert(R < Qry)
assert(H < W && W < Y)

assert ((D <? R).Value)
assert ((D <? Qry).Value)
assert ((R <? Qry).IsNone) // explicitly undefined
assert ((Qry <? R).IsNone) // explicitly undefined
assert (not (D <? D).Value)
assert (not (R <? R).Value)
assert (not (Qry <? Qry).Value)

assert ((D <=? D).Value)
assert ((R <=? R).Value)
assert ((Qry <=? Qry).Value)

let maxD (d1:D) (d2:D) = Syntax.maxD
  
let supD = Syntax.supD 
let maxB (b1:B) (b2:B) = Syntax.maxB

let det t = Syntax.det t           
let supT T d = Syntax.supT T d

type Error = bool

type ExprTyped = Exp

and TargetType   = ColumnType
 
type ModelType =   (ColumnType * ColumnType) * ColumnType
type ModelTyped = Model

type Var = string

// <H, W, X, Y * Z>
type TableType = RecordType * RecordType * RecordType * RecordType * RecordType

type SchemaType =   RecordType * RecordType * RecordType * RecordType * RecordType

//type TableUntyped<'T>    = {Columns: List<ColumnName * ColumnTyped<'T>>}
//and TableTyped<'T>    = TableUntyped<'T> * TableType
//and ColumnTyped<'T>    = {Type:TargetType; Markup:'T}                            


let EmptyRecordType : RecordType = []//T_Record ([])

//shorthands
let ERT = EmptyRecordType

(*
type TableT = TableTyped<MarkupTyped>

type LibEntryUntyped = {CName: string; Table: TableT;  E: ExprTyped}
type LibEntryTyped = LibEntryUntyped * ModelType

type DatabaseType = TargetType * TargetType * TargetType * TargetType * TargetType
type DatabaseUntyped<'T> = {Name: SchemaName; Tables: List<TableName * TableTyped<'T>>}
type DatabaseTyped<'T> = DatabaseUntyped<'T> * DatabaseType
type DatabaseT = DatabaseTyped<MarkupTyped>
*)
type Env = 
    G_Empty
  | G_Var of (Var * (TargetType * B)) * Env
  | G_Table of (Var * TableType) * Env
  | G_Model of (Var * (TableType * List<VarName*Exp> * B)) * Env

let envInsertVar (g:Env) (x:Var) (t:TargetType * B) : Env =
    G_Var ((x, t), g)

let envInsertTable (g:Env) (x:Var) (t:TableType) : Env =
    G_Table ((x, t), g)

let envInsertModel (g:Env) (x:Var) (t:TableType * List<VarName*Exp>  * B) : Env =
    G_Model ((x, t), g)

let extractType (e:ExprTyped) : TargetType =
    let (TypedExp(_, t)) = e in t

let rec getType (g:Env) (x:Ident) : (TargetType * B) =
    match g with
      G_Empty -> failwith (sprintf "Variable %O not in environment" x)
    | G_Var ((y, t), tail) -> if (x=y) then t else getType tail x
    | G_Table ((y, t), tail) -> getType tail x
    | G_Model ((y, t), tail) -> getType tail x

let rec getTableType (g:Env) (x:Ident) : TableType =
    match g with
      G_Empty -> failwith (sprintf "Table %O not in environment" x)
    | G_Var ((y, t), tail) -> getTableType tail x
    | G_Table ((y, t), tail) -> if (x=y) then t else getTableType tail x
    | G_Model ((y, t), tail) -> getTableType tail x

let rec getModelType (g:Env) (x:Ident) : (TableType * List<VarName*Exp> * B) =
    match g with
      G_Empty -> failwith (sprintf "Function %O not in environment" x)
    | G_Var ((y, t), tail) -> getModelType tail x
    | G_Table ((y, t), tail) -> getModelType tail x
    | G_Model ((y, t), tail) -> if (x=y) then t else getModelType tail x

let rec hasVar (g:Env) (x:Ident) : bool =
    match g with
      G_Empty -> false
    | G_Var ((y, t), tail) -> if (x=y) then true else hasVar tail x
    | G_Table ((y, t), tail) -> hasVar tail x
    | G_Model ((y, t), tail) -> hasVar tail x

let rec hasTable (g:Env) (x:Ident) : bool =
    match g with
      G_Empty -> false
    | G_Var ((y, t), tail) -> hasTable tail x
    | G_Table ((y, t), tail) -> if (x=y) then true else hasTable tail x
    | G_Model ((y, t), tail) -> hasTable tail x

let rec hasVarOrTable (g:Env) (x:Ident) : bool =
    match g with
      G_Empty -> false
    | G_Var ((y, t), tail) -> if (x=y) then true else hasVarOrTable tail x
    | G_Table ((y, t), tail) -> if (x=y) then true else hasVarOrTable tail x
    | G_Model ((y, t), tail) -> if (x=y) then true else hasVarOrTable tail x

let rec lookupFieldType (record:List<Var * TargetType>) (fIn:Var) : TargetType =
    match record with
      (f,t) :: tail ->
        if (f = fIn) then t else lookupFieldType tail fIn
    | [] -> failwith (sprintf "no such field: %O" (fIn))

let rec printEnv (g:Env) =
    match g with
      G_Empty -> printf ";;\n"
    | G_Var ((y, t), tail) ->  printf "v>%s\n" y;
                               printEnv tail
    | G_Table ((y, t), tail) -> printf "t>%s\n" y
                                printEnv tail
    | G_Model ((y, t), tail) -> printf "m>%s\n" y
                                printEnv tail
