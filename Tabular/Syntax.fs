namespace MicrosoftResearch.Infer.Tabular


module Syntax = 
  type SchemaName = string
  type TableName  = string
  type ColumnName = string
  type VarName = string
  type FunName = string

  type Factor = FactorName of string

  type Prim = 
            | Negate | Not (* | Exp | Logistic *)
            | Plus | Minus | Mult | Div | Max | Mod
            | Or | And
            | Eq | Neq | Lt | Gt | LtEq | GtEq
            | Factor of Factor
 
  type Dist = 
    | Beta | BetaFromMeanAndVariance 
    | GaussianFromMeanAndPrecision | GaussianFromMeanAndVariance
    | GammaFromShapeAndScale | GammaFromMeanAndVariance | GammaFromShapeAndRate
    | Binomial
    | VectorGaussianFromMeanAndVariance | VectorGaussianFromMeanAndPrecision 
    | Discrete | DiscreteUniform
    | Poisson
    | Bernoulli
    | Dirichlet | DirichletUniform | DirichletSymmetric
    | WishartFromShapeAndRate | WishartFromShapeAndScale

  type real = float

  // spaces
  type D = D | R | Qry 
      with override this.ToString() = match this with D -> "det" | R -> "rnd" | Qry -> "qry"

  let (<?) d1 d2 =
      match d1, d2 with
      | D,R -> Some true
      | D,Qry -> Some true
      | R,Qry -> None
      | Qry,R -> None
      | _ -> Some false

  let (<=?) d1 d2 = 
      if d1 = d2 then Some true else d1 <? d2
  
  // level
  type B = H | W | Y

  type RecordType = List<VarName * ColumnType>
  
  and BaseType =    | B_Link of TableName 
                    | B_Real 
                    | B_Bool 
                    | B_Int 
                    | B_Upto of Exp
                    | B_String 
                    | B_Date
                    | B_Vector (* of Exp *)
                    | B_PositiveDefiniteMatrix
  and  ColumnType   = 
                    | T_Array of ColumnType * Exp
                    | T_Record of RecordType
                    | T_Det of BaseType * D

  and Constant =
                 | IntConst of int
                 | RealConst of real 
                 | BoolConst of bool
                 | StringConst of string
  and Exp =    Var of VarName 
             | Const of Constant
             | Prim of Prim * (Exp list)
             | Dist of Dist * (Exp list)
             | SizeOf of TableName
             | DeRef of Exp * TableName * ColumnName //as defined in the spec, parsed TableName will be "", resolved by type checking
             | Ref of TableName * ColumnName
             | If of Exp * Exp * Exp
             | ForLoop of VarName * Exp * Exp
             | Array of Exp list      //Deprecated 
             | Subscript of Exp * Exp
             | Let of VarName * Exp * Exp
             | Infer of Dist * Exp list * VarName * Exp // more restricted than paper
             | Scan of VarName * VarName * Exp * Exp * Exp // scan (fun(s,x)->E, Einit, Earray) //Experimental
             | Constraint of Exp * ColumnType // e : T
             | TypedExp of Exp * ColumnType
  and Model   = 
             | MEmpty
             | MExp of Exp
             | MIndexed of Model * (*index*) Exp * (*bound*) Exp   // indexed, parsed bound is -1 when ommitted in source, resolved by type checking
             | MCall of FunName * List<(VarName * Exp)> 
             | TypedModel of Model * ((ColumnType * ColumnType) * ColumnType)

  type Markup = (Level * Visibility * Model)
      
  
  and Table    = List<ColumnName * Column> 
  and IdStrategy = | FromPosition | FromColumn of string
  and ID       = | ColumnName of string
  and Column   = {Type:ColumnType; Markup: Markup}                            
  
  and Visibility = In | Local | Output of bool (*true if Latent*)
  and Level = Instance | Static     
  
  (*
  type Markup     = Input                  // column treated as input, no model, just observed data, not predictable
                  | Latent of Model        // column treated as output, not observed in data, predictable
                  | Observable of Model    // column treated as output, observed in data, predictable
                  | Hyper of Exp
                  | Param of Model*)
  let Input : Markup = (Instance,Visibility.In,MEmpty)
  let (|Input|_|) (mup:Markup) = match mup with (Instance,Visibility.In,_) -> Some () | _ -> None
  let Param m = (Static,Visibility.Output true,m)
  let (|Param|_|) mup = match mup with (Static,(Visibility.Output true| Visibility.Local), m)  -> Some m | _ -> None
  let Latent m = (Instance,Visibility.Output true,m)
  let (|Latent|_|) mup = match mup with (Instance,(Visibility.Output true| Visibility.Local), m)  -> Some m | _ -> None
  let Observable m = (Instance,Visibility.Output false,m)
  let (|Observable|_|) mup = match mup with (Instance,Visibility.Output false, m)  -> Some m | _ -> None
  let Hyper e = (Static,Visibility.In,
                 match e with // preserve typing, if present
                 | TypedExp (_,t) -> TypedModel(MExp e,((T_Record[],T_Record[]),t))
                 | e -> MExp e)
  let (|Hyper|_|) mup = match mup with 
                        | (Static,Visibility.In, m)  -> 
                           match m with // ignore typing, if present
                           | TypedModel(MExp e,_) | MExp e -> 
                              Some e
                           | _ -> None
                        | _ -> None
  
  type TableId = 
      | Table of TableName * IdStrategy option
      | Fun of FunName
      with member x.Name       = match x with | Table (n,_) | (Fun n) -> n

  type Declaration =  |Declaration of TableId * Table
      with member x.getColumns = match x with | Declaration(_,t) -> t
           member x.getTable   = x.getColumns   
           member x.Name       = let (Declaration (x,_)) = x in x.Name



  let getColumnsFromDec (d:Declaration) = d.getColumns 

  type Schema = List<Declaration>
   

  // Derived constructors and destructors for base types
  // NB: constructors produce deterministic types but destructors ignore determinacy

  let T_Int =  T_Det(B_Int,D)
  let (|T_Int|_|) t = match t with T_Det(B_Int,_) -> Some () | _ -> None
  
  let T_String =  T_Det(B_String,D)
  let (|T_String|_|) t = match t with T_Det(B_String,_) -> Some () | _ -> None
 
  let T_Real =  T_Det(B_Real,D)
  let (|T_Real|_|) t = match t with T_Det(B_Real,_) -> Some () | _ -> None
 
  let T_Bool =  T_Det(B_Bool,D)
  let (|T_Bool|_|) t = match t with T_Det(B_Bool,_) -> Some () | _ -> None

  let T_Vector = T_Det(B_Vector,D)
  let (|T_Vector|_|) t = match t with T_Det(B_Vector,_) -> Some () | _ -> None

  let T_PositiveDefiniteMatrix = T_Det(B_PositiveDefiniteMatrix,D)
  let (|T_PositiveDefiniteMatrix|_|) t = match t with T_Det(B_PositiveDefiniteMatrix,_) -> Some () | _ -> None
  
  let T_Upto e = T_Det(B_Upto e,D)
  let (|T_Upto|_|) t = match t with T_Det(B_Upto(e),_) -> Some e | _ -> None
  let T_Link tn = T_Det(B_Link tn,D)
  let (|T_Link|_|) t = match t with T_Det(B_Link tn,_) -> Some tn | _ -> None
 
  
  let maxD (d1:D) (d2:D) =
    match d1 <=? d2 with 
    | Some leq -> if leq then Some d2 else Some d1
    | None -> None
  
  let supD = maxD

  let maxB (b1:B) (b2:B) = max b1 b2

  let rec makeDet T d =  
    match T with 
    | T_Det(b,_) -> T_Det(b,d)
    | T_Array (T',e) -> T_Array (makeDet T' d,e)
    | T_Record tys -> T_Record (List.map (fun (fld,T) -> (fld,makeDet T d))  tys)

  let rec det T =  
    match T with 
    | T_Det(b,d) -> d
    | T_Array (T',e) -> det T'
    | T_Record tys -> 
        List.fold (fun d (fld,T) ->  match supD d (det T) with Some d -> d | _ -> failwith "detT: impossible") D tys

  let rec supT T d =  
    match T with 
    | T_Det(b,d') -> 
      match supD d' d with
      | Some sup ->  Some (T_Det(b,sup))
      | None -> None
    | T_Array (T',e) -> 
      match supT T' d with 
      | Some T'' -> Some (T_Array(T'', e))
      | None -> None
    | T_Record tys ->
      let rec supTs acc Ts = 
              match tys with
              | [] -> Some (T_Record (List.rev acc))
              | (fld,T)::Ts ->
                 match supT T d with
                 | Some T' -> supTs ((fld,T')::acc) Ts
                 | None -> None
      supTs [] tys
 
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Table = 
   let OnlyConcreteDataColumns (t:Table)= 
       t |>  List.filter (fun (_, ct)  -> match ct.Markup, ct.Type with 
                                          | Input, T_Det(bt,_) | Observable(_),T_Det(bt,_) -> true
                                          | _ -> false)
   let OnlyLinks(t:Table)= 
       t |>  List.filter (fun (_, ct)  -> match ct.Type with 
                                          | (T_Link table) -> true
                                          | _ -> false)
   let GetColNames (t:Table)= 
       t  |> List.map fst


  let level ((cn,{Type=T;Markup=m}):ColumnName * Column) = 
      match m with 
         | Hyper _ -> H 
         | Param _ -> W
         | _ -> Y
  

  let DependsOnTables (table:Declaration)  = table |> getColumnsFromDec |> List.choose(fun (colname, col) -> match col.Type with | T_Link a -> Some a | _ -> None )



  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Schema = 
      let OnlyTables (s:Schema ) = s |> List.choose (function | Declaration(Table(tname, _),_) as t -> Some t | _ -> None) //only real table names
      let GetNames   (s:Schema ) = s |> List.map    (function | Declaration(Table(tname, _),_)  -> tname | Declaration(Fun(fname),_) -> fname)

      let onlyTableInputAndObservables schema :Schema  = 
            schema |> OnlyTables 
                   |> List.map (fun t ->  let ((Declaration(Table(a, b), cols))) = t
                                          Declaration(Table(a, b), Table.OnlyConcreteDataColumns cols )) 

      let GetTableByName (s:Schema) name = 
               try ( let ts = s |> List.find(function | Declaration(Table(tname, _),_)| Declaration(Fun(tname), _) -> tname = name )
                     //let tst = ts.getTable
                     ts)
               with | e -> failwithf "table %A not found" name

      let GetNameListTS (s:Schema) = 
         s |> GetNames 
           |> SchemaGraph.ItemsTS (GetTableByName s) (DependsOnTables) id    //sorted topologically

      let getTableCol s tname cname = 
        match tname 
                |> GetTableByName s 
                |> getColumnsFromDec
                |> List.tryPick (fun (cn,col) -> if cname=cn then (Some col) else None) with 
        | Some col -> col
        | None -> failwithf "table %A does not have a column named %A" tname cname

  

  let (|Concrete|_|) (t:Column) = match t.Markup with |Input | Observable(_) -> Some () | _ -> None

  // substitution (TODO: remember that types are dependent!)

  let mkCDiscreteFun n =
    ("CDiscrete",
           ["N", {Type=T_Int;  Markup=Hyper(Const (IntConst n))};
            "V", {Type=T_Vector;
                  Markup=Param(MExp(Dist(Dirichlet,[Var "N"; ForLoop ("i", Const(IntConst n), Const (RealConst 1.0))])))};
            "CDiscrete", {Type=T_Upto (Exp.Var "N"); 
                              Markup=Observable(MExp(Dist(Discrete,[Const(IntConst n); Var "V"])))} ])

  let mutable counter = 0;
  let fresh x = counter <- counter+1; 
                "v___" + counter.ToString();

  let rec substE ((ex,x) as exx) =
    //todo: fix binding cases
    let rec sub e =
      match e with
      | Var(y) -> if x=y then ex else e
      | Const(k) -> Const(k)
      | Prim(p,es) -> Prim(p,List.map sub es)
      | Dist(d,es) -> Dist(d,List.map sub es)
      | SizeOf t -> SizeOf t
      | DeRef(e1,t,nme) -> DeRef(sub e1,t,nme)
      | Ref(t,nme) -> Ref(t,nme)
      | If(e1,e2,e3) -> If(sub e1,sub e2,sub e3)
      | ForLoop(x,e1,e2) -> 
        let x' = fresh x
        ForLoop(x',sub e1, sub (substE (Var x',x)  e2))
      | Array(es) -> Array(List.map sub es)
      | Subscript(e1,e2) -> Subscript(sub e1,sub e2)
      | Constraint(e1,t1) -> Constraint(sub e1,substT exx t1)
      | Let(x,e1,e2) -> //failwith "substE: Let not implemented - needs occurs check" 
        let x' = fresh x 
        Let(x',sub e1, sub (substE (Var x',x)  e2))
      | Infer(d,es,x,e) ->
        Infer(d,List.map sub es, x, sub e) 
      | TypedExp(e1,t1) -> TypedExp(sub e1, substT exx t1)
    sub

  and substM exx M =
    match M with
    | MEmpty -> MEmpty
    | MExp(e) -> MExp(substE exx e)
    | MIndexed(M1,e1,e2) -> MIndexed(substM exx M1, substE exx e1, substE exx e2)
    | MCall(f,args) -> MCall(f,List.map (fun(v,e) -> (v,substE exx e)) args)
    | TypedModel(M,((ty1,ty2),ty3)) -> TypedModel(substM exx M, ((substT exx ty1,substT exx ty2),substT exx ty3))

  and substA exx ((l,v,M):Markup) = (l,v,substM exx M)
    
  and substB exx b = 
    match b with
    | B_Upto e  -> B_Upto (substE exx e)
    | B_Vector (* e *) -> B_Vector  (* explicit case in case we decide to index B_Vector *)
    | B_PositiveDefiniteMatrix (* e *)  -> B_PositiveDefiniteMatrix  (* explicit case in case we decide to index B_PositiveDefiniteMatrix *)
    | b -> b
  and substT exx T= 
    match T with 
    | T_Array (t,e) -> T_Array (substT exx t, substE exx e)
    | T_Record rt -> T_Record (List.map (fun (c,t) -> (c,substT exx t)) rt)
    | T_Det (b,d) -> T_Det (substB exx b,d)
  and substRT exx (rt:RecordType) =  List.map (fun (c,t) -> (c,substT exx t)) rt
  and substC (ex,x) col =
    match col with {Type=ty; Markup=A} -> {Type=substT (ex,x) ty; Markup=substA (ex,x) A}


  
  let rename (ex,x) nme =
    if x<>nme then nme else
    match ex with
    | Var(y) -> y
  //  | Const(e) -> x
    | _ -> failwith (sprintf "rename: %s %s expected variable but got other expression" x nme)

  let substNC (ex,x) (nme,col) = (rename (ex,x) nme, substC (ex,x) col)

    
  let update_col e col =
    match col with
    | {Type=tau;Markup=(l,In,_)} -> {Type=tau;Markup= (l,Local,MExp(e))}
    | _ -> col

  let qualify nme nme' = nme+"_"+nme'
  // given schema s, expand out nme -> MCall(f,args)

  // TODO: rewrite me inductively
  let beta (FE:Map<FunName,Table>) nme f args =
    let T1:Table = List.rev(FE.[f]) 
    let T2 = List.fold (fun (T:Table) (nme,e) -> List.map (fun(nme',col) -> nme', if nme=nme' then update_col e col else col) T) T1 args
    let (f',last),rest = List.head T2, List.rev (List.tail T2)
    if f <> f' && f' <> "ret" then failwith (sprintf "expected column %s but got %s" f f') else
    let exxs = List.map (fun col -> 
                         match col with
                         // inline Hypers
                         | (nme',{Type=ty;Markup = (Static,Local,MExp(e'))}) when det ty = D-> (e', nme') 
                         // rename others
                         | (nme',_) -> (Var(qualify nme nme'), nme') 
                         ) rest // the substitutions
    let sigma T = 
        // remove inlined Hypers
        let T = List.filter (function
                               (_,{Type=ty;Markup=(Static,Local,MExp(e'))}) when det ty = D -> false 
                              | _ ->true) T
        List.fold (fun T' (ex,x) -> List.map (substNC (ex,x)) T') T exxs
    (sigma rest, List.head (sigma [(nme, last)]))

  let makeParam (nme,col) =
    nme,
    match col with
    | {Type=tau;Markup=(l,In,_) as mup} -> failwithf "unexpected markup %A after beta" mup
    | {Type=tau;Markup=(Static,_,_) as mup} -> {Type=tau;Markup=mup}
    | {Type=tau;Markup=(Instance,((Local|Output true) as vis),m) as mup} -> {Type=tau;Markup=(Static,vis,m)} //FIXED
    | {Type=tau;Markup=(Instance,Output false,m)} -> {Type=tau;Markup=(Static,Output true,m)} // TBR

  let rec indexT C (n:int) (eIndex, eSize) T =
      let hat c = c+"_"+n.ToString() // can't use ^ for (literal) extraction
      let ArgMax e1 = Prim(Prim.Factor(FactorName "ArgMax"),[e1])
      let recurse = indexT C n (eIndex,eSize) 
      match T with
      | [] -> []
      | ((nme,col) as dec)::T->
         match col with
         | {Type=tau;Markup=(_,In,_)} -> dec::(recurse T)
         | {Type=tau;Markup=(Static,vis,MExp e)} when det tau = R ->
           let i = fresh () // sprintf "_%s_%i" nme n
           let sigma e = Set.fold (fun e' d -> substE (Subscript(Var d,Var i) , d) e') e C
           (nme,{Type=T_Array(tau,eSize);Markup=(Static,vis,MExp( ForLoop(i,eSize,sigma e)  ))})
           ::
           (hat nme,{Type=tau;Markup=(Instance,Local,(MExp(  (Subscript(Var nme,eIndex) ))))})
           ::
           indexT (Set.add nme C) n (eIndex, eSize) T
          | {Type=tau;Markup=(Static,vis, MExp e)} when det tau = Qry ->
           let i = fresh () // sprintf "_%s_%i" nme n
           let sigma e = Set.fold (fun e' d -> substE (Subscript(Var d,Var i) , d) e') e C
           (nme,{Type=T_Array(tau,eSize);Markup=(Static,vis,MExp( ForLoop(i,eSize,sigma e)  ))})
           ::
           (hat nme,{Type=tau;Markup=(Instance,Local, MExp(Subscript(Var nme,ArgMax(Infer(Discrete,[eSize],"probs",eIndex)))))})
           ::
           indexT (Set.add nme C) n (eIndex, eSize) T
         | {Type=tau;Markup=(Static,vis, MExp e)} when det tau = D ->
           let sigma e = Set.fold (fun e' d -> substE (Var (hat d), d) e') e C
           // assert (e = sigma e) up to alpha
           (nme,{Type=tau;Markup=(Static,vis,MExp(sigma e))}) :: (recurse T)
         | {Type=tau;Markup=(Instance,vis, MExp e)} -> 
           let sigma e = Set.fold (fun e' d -> substE (Var (hat d), d) e') e C
           (nme,{Type=tau;Markup=(Instance,vis, MExp (sigma e))}) :: (recurse T) 
         |  _ -> failwith "indexC: not core Tabular"

  let rec morph n FE nc =
      match nc with
      | (nme, {Type=tau; Markup=(Static,viz,MCall(f,args))}) ->
          match beta FE nme f args with
          | (T,(nme',{Type=tau';Markup=(Instance,Output false, M)})) -> 
            List.map makeParam (T @ [(nme',{Type=tau';Markup=(Instance,Output true, M)})])
      | (nme, {Type=tau; Markup=(Instance, Output true, MCall(f,args))}) ->
          match beta  FE nme f args with
          | (T,(nme',{Type=tau';Markup=(Instance,Output false, M)})) -> T @ [(nme',{Type=tau';Markup=(Instance,Output true, M)})]
      | (nme, {Type=tau; Markup=(Instance,((Output false|Local) as vis),MCall(f,args))}) -> (* same as previous case? *)
          match beta FE nme f args with
          | (T,(nme',{Type=tau';Markup=(Instance,Output false, M)})) -> T @ [(nme',{Type=tau';Markup=(Instance,vis,M)})]
      | (nme, {Type=tau; Markup=(Instance,((Local|Output _) as vis),MIndexed(M,eIndex,eSize))}) ->
        let T:Table = morph (n+1) FE (nme, {Type=tau; Markup=(Instance,vis,M)})
        indexT Set.empty n (eIndex,eSize) T
      | (nme, {Type=tau; Markup=(Static,((Local|Output _) as vis), MIndexed(M,eIndex,eSize))}) ->
        let T:Table = morph (n+1) FE (nme, {Type=tau; Markup=(Instance,vis,M)}) 
        List.map makeParam (indexT Set.empty n (eIndex,eSize) T)
      | (nme, col) -> [nme,col] 
  
  let coreT (FE:Map<FunName,Table>) (T:Table):Table = List.concat (List.map (morph 0 FE) T)
 
  let coreS (S:Schema): Schema =
    let rec coreS FE S =
        match S with
        | [] -> []
        | Declaration(Table(t,oId),T)::rest->
          let T' = coreT FE T
          Declaration(Table(t, oId),T'):: coreS FE rest
        | Declaration(Fun f,T)::rest ->
          let T' = coreT FE T
          let FE' = FE.Add(f,T')
          coreS FE' rest
    in
       coreS Map.empty S
   