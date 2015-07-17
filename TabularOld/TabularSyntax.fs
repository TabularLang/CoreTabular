namespace MicrosoftResearch.Infer.Tabular
open System

module Tabular = 
  type SchemaName = string
  type TableName  = string
  type ColumnName = string
  type Var = string

#if UNITBUGFIXED
  type UNIT = unit
  val ONE = ()
#else
  type UNIT = bool
  let ONE:UNIT = true
  //type UNIT = int
  //let ONE:UNIT = 666
#endif
    
  type Prim = 
        | PEQ 
        | PMinus 
        | PGT
  type Dist = 
        | DBernoulli 
        //...
  type real = float
  type ColumnType   = T_Link of TableName 
                    | T_Real 
                    | T_Bool 
                    | T_Int 
                    | T_Upto of Expr
                    | T_String 
                    | T_Array of ColumnType * Expr
                    | T_Record of List<Var * ColumnType>
                    | T_Vector 
  
  and Expr   = 
              | Const of int
              | DiscreteConst of int * int    // Hack: bounded constant
              | RealConst of real
              | BoolConst of bool
              | GT of Expr * Expr
              | SizeOf of TableName
              | Column of ColumnName
              | Deref of ColumnName * Expr // Expr restricted to Column ColumnName
                                           //Discrepancy with the specification- first argument
                                           //should be an Expr according to spec
              | FullDeref of Expr * ColumnName //as defined in the spec
              | Param of TableName * ColumnName // is this in paper?
              | If of Expr * Expr * Expr
              | StrictIf of Expr * Expr * Expr // workaround needed for DA model
              | EQ of Expr * Expr
              | Minus of Expr * Expr
              | Plus of Expr * Expr
              | Times of Expr * Expr
              | Or of Expr * Expr
              | Arr of Expr []
              | Index of Expr * Expr // array indexing
              | Gaussian of Expr * Expr
              | Gamma of Expr * Expr
              | DampBackward of Expr * Expr
              | Logistic of Expr //* Expr
              | Probit of Expr * Expr //* Expr
              | DiscreteUniform of Expr //* Expr
              | Bernoulli of Expr
              // NYI
              | Let of Var * Expr * Expr
              | Var of Var
              | Prim of Prim * (Expr list)
              | Dist of Dist * (Expr list)
              //NYI- need this for checking models
              //Here, records are represented as lists
              | Record of List<(Var * Expr)>
              | ForLoop of Var * Expr * Expr
              //additional distributions
              | Dirichlet of Expr * Expr //? Vector
              | Beta of Expr * Expr
              | Discrete of Expr * Expr
              // From Tabular Spec
              | HyperParam of TableName * ColumnName
           

  type Model  = CDiscreteWith of Expr
              | CDiscrete
              | CBernoulli
              | CGaussian
              | String
              | Array of Model * Expr list      //Deprecated 
              | Gen of Expr
              | WithPrior of Model * Expr
              // NYI
              | Indexed of Model * Expr * int   // indexed
              | ArrayModel of Var * int * Model //array-valued models
              // Should it be: "| ArrayModel of Var * Expr * Model"  ?
              | Static of Model 
              | Prior of Expr
              | Hyper of Expr
              | With of Model * ColumnName * Expr // this need to be reviewed if we haven't got record types
              | LetRow of Var * Var * Table<Model> * Expr // this needs to be reviewed

             (*| Model of Expr * (Var * Expr) * (var * var * Expr) *) 

(* A simpler letrow?
   G_w, G_y |-row T : <H,W,{},{},Z>  G(Z) |- E : T
   ---------------------------------------------
       G_w,G_y |- letrow T in E : Model<H,W,{},T>

   T $ <Eh,(h)Ew,(w,x)Eyz>  
   --------------------------
   letrow T in E $ <Eh,(h)Ew,(w,x)(let (_,z) = Eyz in (let ci=z.ci) i \in L(T) in E>

*)
  and Table<'T>    = {Columns: List<ColumnName * Column<'T>>} 
  and Column<'T>   = {Type:ColumnType; Markup:'T}                            
                
  type Database<'T> = {Name: SchemaName; Tables: List<TableName * Table<'T>>} 

  type LibEntry<'T>= {CName: string; Table: Table<'T>;  E: Expr}
  
  type Markup     = Input                  // column treated as input, no model, just observed data, not predictable
                  | Latent of Model        // column treated as output, not observed in data, predictable
 
                  | Observable of Model    // column treated as output, observed in data, predictable
 (*
 
 (*
  Should we decide to refactor...
 *)
                  | MHyper of Expr
                  | MParam of Model

  
 
  let GT (e1,e2) = Expr.Prim(Prim.GT,[e1;e2])
  let (|GT|_|) e = match e with (Expr.Prim(Prim.GT,[e1;e2])) -> Some (e1,e2) | _ -> None
  
  let Latent m = match m with Hyper e -> MHyper(e) | Prior e -> MParam(Gen(e)) | m -> Latent m 
  let (|Latent |_|) m = match m with (Markup.Latent m) -> Some m | _ -> None
  let Observable m = match m with Hyper e -> MHyper(e) | Prior e -> MParam(Gen(e)) | m -> Latent m 
  let (|Observable |_|) m = match m with (Markup.Observable m) -> Some m | _ -> None
  *)

  type Table<'T>    with 
    member x.Items                = x.Columns
    member x.DependsOn otherTable = x.Items |> Seq.exists(fun (colname, col) -> match col.Type with | T_Link linkedtable when linkedtable = otherTable -> true | _ -> false )
    member x.DependsOnTables      = x.Items |> Seq.choose(fun (colname, col) -> match col.Type with | T_Link a -> Some a | _ -> None )
    member x.ForeignKeysColName   = x.Items |> Seq.choose(fun (colname, col) -> match col.Type with | T_Link a -> Some (colname, a) | _ -> None )
    member x.IsForeignKeyColName  = 
        let fk = x.ForeignKeysColName |> Set.ofSeq
        fun name -> fk.Contains name
    //updating models 
    member x.Add(columns: List<ColumnName * Column<'T>>) = 
      let names = x.Columns |> Map.ofSeq
      let tomerge, toadd = columns |> List.partition(fun (newname,_) -> names.ContainsKey newname) |> (fun (a,b) -> a  |> Map.ofSeq, b)
      {Columns =  List.append (x.Columns |> List.map (fun (name, column) -> name, {Type = column.Type; Markup =  if tomerge.ContainsKey name then (tomerge.[name]).Markup else column.Markup}))
                               toadd }
    member x.ColumnSubsetChoose(filter) =  { Columns = [for  (name, col)  in x.Columns do 
                                                          if filter(box col.Markup :?> Markup) then 
                                                            yield name, col ] }
    member x.ConcreteSubset   = x.ColumnSubsetChoose (fun m -> match m  with | Observable(_) | Input -> true  | Latent(_) -> false)
    member x.LatentSubset     = x.ColumnSubsetChoose (fun m -> match m  with | Observable(_) | Input -> false | Latent(_) -> true)
    member x.ObservableSubset = x.ColumnSubsetChoose (fun m -> match m  with | Observable(_)  -> true  | _ -> false)

  
  module Table = 
    let ConcreteSubset   (x:Table<_>) = x.ConcreteSubset
    let LatentSubset     (x:Table<_>) = x.LatentSubset
    let ObservableSubset (x:Table<_>) = x.ObservableSubset

  module Graph = 
   let ItemsTS getTableByName getDependencies projection x = 
      let visited = ref Set.empty
      let rec dfs (tableName:string) (visited:Set<_> ref) = [
           if not ((!visited).Contains tableName) then
             visited := (!visited).Add tableName
             let table = getTableByName tableName
             for t in getDependencies(table) do
              yield! dfs t visited
             yield projection tableName  ]

      x |> List.fold(fun (visited, previousseq) (tname) -> (visited, (dfs tname visited)@previousseq)) (visited, List.empty) |> snd |> List.rev

//
//  module Database = 
//    let empty = {Name=""; Tables=List.empty}
//    let private ColumnSubsetChoose(filter) db =  [ for (tname, t) in db.Tables -> tname, t.ColumnSubsetChoose filter ]
//    let ConcreteSubset    db = {Name =db.Name; Tables= [ for (tname, t) in db.Tables -> tname, t.ConcreteSubset ]}
//    let LatentSubset      db = {Name =db.Name; Tables= [ for (tname, t) in db.Tables -> tname, t.LatentSubset     ]}
//    let ObservableSubset  db = {Name =db.Name; Tables= [ for (tname, t) in db.Tables -> tname, t.ObservableSubset ]}
//    let Add(tables: List<TableName * Table<'T>>) x = 
//      let names = x.Tables |> Map.ofSeq
//      let tomerge, toadd = tables |> List.partition(fun (newname,_) -> names.ContainsKey newname) |> (fun (a,b) -> a  |> Map.ofSeq, b)
//      {x with Tables = List.append (x.Tables |> List.map (fun (name, table) -> name, if tomerge.ContainsKey name then table.Add (tomerge.[name].Columns) else table ))
//                                    toadd }
//    let SubsetChoose(filter) x = 
//        { x with Tables = x.Tables|> List.filter (fun (tname, t) ->  (t.ColumnSubsetChoose filter).Items |> Seq.isEmpty |> not) }
//    let OnlyTableWithConcrete x = SubsetChoose (fun m -> match m  with | Observable(_) | Input -> true  | Latent(_) -> false) x
//    let OnlyTableWithLatent x  = SubsetChoose (fun m -> match m  with | Observable(_) | Input -> false | Latent(_) -> true) x
//
//    let GetTableByName x name = try (x.Tables |> List.find(fun (tname, table) -> tname = name) |> snd) with | e -> failwith (sprintf "table %A not found" name)
//    let Items x = x.Tables
//
//      
//    let ItemsTSName x      = 
//      Graph.ItemsTS (GetTableByName x) (fun (table:Table<_>) -> table.DependsOnTables) id (Items x |> List.map fst)
//    let ItemsTSNameTable x = 
//      Graph.ItemsTS (GetTableByName x) (fun (table:Table<_>) -> table.DependsOnTables) (fun tablename -> tablename, GetTableByName x tablename) (Items x |> List.map fst)
//    

//  let nl = System.Environment.NewLine
//  let rec ident n = n
//  let rec expToTex e =
//      match e with
//        | Const i -> i.ToString()
//        | DiscreteConst (i,n) ->  sprintf "%O(%O)" i n                                  
//        | RealConst r -> sprintf "%A" r
//        | BoolConst b -> b.ToString()
//        | GT (e1,e2) -> sprintf "%O > %O" (expToTex e1) (expToTex e2)
//        | SizeOf t-> sprintf "sizeof(%s)" (ident t)
//        | Column c -> ident(c)
//        | Deref (c,e) -> sprintf "%O.%O" (ident c) (expToTex e)
//        | Param (t,n) -> sprintf "%O.%O" (ident t) (expToTex e)
//        | If(e1,e2,e3)  
//        | StrictIf (e1,e2,e3)->  sprintf "if %O then %O else %O" (expToTex e1) (expToTex e2) (expToTex e3) 
//        | EQ (e1,e2)  -> sprintf "%O = %O" (expToTex e1) (expToTex e2)
//        | Minus (e1,e2)  -> sprintf "%O - %O" (expToTex e1) (expToTex e2)
//        | Or (e1,e2)  -> sprintf "%O | %O" (expToTex e1) (expToTex e2)
//        | Arr es -> sprintf "\\[ %O \\]"  (expsToTex (Array.toList(es)))
//        | Index  (e1,e2) ->  sprintf "%O[%O]" (expToTex e1) (expToTex e2)
//        | Gaussian(e1,e2) ->  sprintf "Gaussian(%O,%O)" (expToTex e1) (expToTex e2)
//        | Gamma(e1,e2) -> sprintf "Gamma(%O,%O)" (expToTex e1) (expToTex e2)
//        | DampBackward(e1,e2) -> sprintf "DB(%O,%O)" (expToTex e1) (expToTex e2)
//        | Probit(e1,e2) -> sprintf "Probit(%O,%O)" (expToTex e1) (expToTex e2)
//        | DiscreteUniform e1 -> sprintf "DiscreteUniform(%O)" (expToTex e1) 
//        | Bernoulli e2  -> sprintf "Bernoulli(%O)" (expToTex e2) 
//       
//
//
//
//  and expsToTex es = 
//      match es with 
//      | [] -> ""
//      | e::es -> sprintf "%O,%O" (expToTex e) (expsToTex es)   
//  let rec modelToTex m =
//      match m with
//      | CDiscreteWith e ->
//        sprintf "CDiscreteWith with N=%O" (expToTex e)                                   
//      | CBernoulli ->
//        "CBernoulli" 
//      | CGaussian ->
//        "CGaussian"
//      | String ->
//        "String"
//      | Array (m,es) ->
//         sprintf "(%O)\\[%O\\]" (modelToTex m) (expsToTex es) 
//      | Gen e ->
//         sprintf "gen (%O)" (expToTex e)
//      | WithPrior (m,e) ->
//         sprintf "%O withprior (%O)" (modelToTex m) (expToTex e)
//  let rec columnTypeToTex ty =
//      match ty with
//      | T_Real -> "real"
//      | T_Int -> "int"
//      | T_Bool -> "bool"
//      | T_String -> "string"
//      | T_Link t -> sprintf "link(%O)" t
//      | T_Array (t,e) -> sprintf "%O\\[\%O\]" t (expToTex e)
//      | T_Upto e -> sprintf "Upto(%O)"  (expToTex e)
//  let columnToTex (cn,{Type=ty;Markup=m}) =
//      match m with
//      | Input ->
//        sprintf "%O \\InputField{%O}{%O} \\\\" nl cn (columnTypeToTex ty)  
//      | Latent m ->
//        sprintf "%O \\LatentField{%O}{%O}{%O} \\\\" nl cn (columnTypeToTex ty) (modelToTex m)
//      | Observable m -> 
//        sprintf "%O \\ObsField{%O}{%O}{%O} \\\\" nl cn (columnTypeToTex ty) (modelToTex m)
//  let tableToTex(tn,this:Table<Markup>) =
//      sprintf "%O{%O}\\\\%O" nl tn (String.concat "" (List.map columnToTex this.Columns))
//     
//  let dataBaseToTex(this:Database<Markup>) = 
//      sprintf "%O%%\\Model{%O}\\\\%O" nl this.Name (String.concat "" (List.map tableToTex this.Tables) )
//       
//
//  let rec 
//    modelToStr (m:Model) : string =
//    match m with
//      | CDiscreteWith e ->
//         modelToStr(With(CDiscrete,"N",e))
//      | CBernoulli ->
//        "CBernoulli" 
//      | CGaussian ->
//        "CGaussian"
//      | String ->
//        "String"
//      | ArrayModel (x, c, n) ->
//          "[for " + x + "<" + (c.ToString()) + "->" + (modelToStr ( n)) + "]"
//      | Gen e ->
//         "gen (" + (exprToStr ( e)) + ")"
//      | WithPrior (n, e) ->
//         (modelToStr ( n)) + " withprior " + (exprToStr ( e))
//      | Static (n) ->
//          sprintf "static(%O)" (modelToStr ( n))
//      | Prior (e) ->
//          sprintf "prior(%O)" (exprToStr ( e))
//      | Indexed (n, e, c) ->
//          sprintf "(%O)[%O<%O]" (modelToStr ( n)) (exprToStr ( e)) (c.ToString())
//      | Hyper (e) ->
//          sprintf "hyper(%O)" (exprToStr ( e))
//      | CDiscrete  ->
//          "CDiscrete"
//      | With(m,c,e) ->
//          (modelToStr ( m)) + " with " + c + " = " +  (exprToStr ( e))
//      | Array (m,es) ->
//         sprintf "(%O)[%O]" (modelToStr m) (expsToStr es) 
//      | _ -> failwithf "cannot print this kind of model yet: %A" m
//
//  and exprToStr (e:Expr) =
//    match e with
//      | Const i -> i.ToString()
//      | DiscreteConst (i,n) ->  sprintf "%O(%O)" i n                                  
//      | RealConst r -> sprintf "%A" r
//      | BoolConst b -> b.ToString()
//      | GT (e1,e2) -> sprintf "%O > %O" (exprToStr ( e1)) (exprToStr ( e2))
//      | SizeOf t-> sprintf "sizeof(%s)" (ident t)
//      | Column c -> ident(c)
//      | Deref (c,e) -> sprintf "%O.%O" (ident c) (exprToStr ( e))
//      | Param (t,n) -> sprintf "%O.%O" (ident t) (ident n)
//      | If(e1,e2,e3)  
//      | StrictIf (e1,e2,e3)->  sprintf "if %O then %O else %O" (exprToStr ( e1)) (exprToStr ( e2))(exprToStr ( e3))
//      | EQ (e1,e2)  -> sprintf "%O = %O" (exprToStr ( e1)) (exprToStr ( e2))
//      | Minus (e1,e2)  -> sprintf "%O - %O" (exprToStr ( e1)) (exprToStr ( e2))
//      | Plus (e1,e2)  -> sprintf "%O + %O" (exprToStr ( e1)) (exprToStr ( e2))
//      | Times (e1,e2)  -> sprintf "%O * %O" (exprToStr ( e1)) (exprToStr ( e2))
//      | Or (e1,e2)  -> sprintf "%O | %O" (exprToStr ( e1)) (exprToStr ( e2))
//      | Arr es -> sprintf "[ %O ]"  (expsToStr (Array.toList(es)))
//      | Index  (e1,e2) ->  sprintf "%O[%O]" (exprToStr ( e1)) (exprToStr ( e2))
//      | Gaussian(e1,e2) ->  sprintf "Gaussian(%O,%O)" (exprToStr ( e1)) (exprToStr ( e2))
//      | Gamma(e1,e2) -> sprintf "Gamma(%O,%O)" (exprToStr ( e1)) (exprToStr ( e2))
//      | DampBackward(e1,e2) -> sprintf "DB(%O,%O)" (exprToStr ( e1)) (exprToStr ( e2))
//      | Probit(e1,e2) -> sprintf "Probit(%O,%O)" (exprToStr ( e1)) (exprToStr ( e2))
//      | DiscreteUniform e1 -> sprintf "DiscreteUniform(%O)" (exprToStr ( e1))
//      | Bernoulli e2  -> sprintf "Bernoulli(%O)" (exprToStr ( e2))
//      | ForLoop (x, e, f)  ->  sprintf "[for %s<%O ->%O]" x (exprToStr ( e)) (exprToStr ( f))
//      | Dirichlet (e1, e2)  -> sprintf "Dirichlet(%O)" (exprToStr ( e2))
//      | Beta (e1, e2)  -> sprintf "Beta(%O,%O)" (exprToStr ( e1)) (exprToStr ( e2))
//      | Discrete(e1,e2) -> sprintf "Discrete(%O,%O)" (exprToStr ( e1)) (exprToStr ( e2))
//      | Let(v,e1,e2) -> sprintf "let %O = %O in %O" (ident v) (exprToStr e1) (exprToStr e2)
//      | Var v -> v
//      | Prim (p,es) -> sprintf "%A(%O)" p (expsToStr es)
//      | Dist (d,es) -> sprintf "%A(%O)" d (expsToStr es)
//      | Record flds ->
//           sprintf "{%O}" (fldsToStr flds)
//      | FullDeref(e,f) ->
//          sprintf "%O.%O" (exprToStr e) (ident f)
//      //| _ -> failwith "(exprToStr) this case not implemented yet"
//  and fldsToStr es = 
//      match es with 
//      | [] -> ""
//      | [(f,e)] -> sprintf "%O=%O" (ident f) (exprToStr ( e))
//      | (f,e)::es -> sprintf "%O=%O,%O" (ident f) (exprToStr ( e)) (fldsToStr es)
//  and expsToStr es = 
//      match es with 
//      | [] -> ""
//      | [e] -> exprToStr e
//      | e::es -> sprintf "%O,%O" (exprToStr ( e)) (expsToStr es)