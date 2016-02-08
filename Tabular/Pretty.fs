namespace MicrosoftResearch.Infer.Tabular

module Pretty =
  module S = MicrosoftResearch.Infer.Tabular.Syntax
 

  let rec private isAtomic e =
      //TBR 
      match e with 
      | S.Var _ | S.Const _ | S.SizeOf _  | S.ForLoop _ | S.Array _ | S.DeRef _ | S.Prim (S.Factor _,_)| S.Dist _ | S.Subscript _  -> true
      | S.TypedExp(e,t) -> isAtomic e
      | _ -> false
      
  let escape = ref (fun (s:string) -> s);  // updated by the parser to escape idents as necessary
  let ident (x:string) = (!escape) x
      
  let uptoAsMod = ref true
  let vectorAsArray = ref (None:int option)

  let detToStr d = 
      match d with 
      | Syntax.D -> "det"
      | Syntax.R -> "rnd"
      | Syntax.Qry -> "qry"

  let rec 
    modelToStr (m:S.Model) : string =
    match m with 
     | S.MEmpty -> ""
     | S.MExp e -> exprToStr e
     | S.MIndexed(m,e1,S.Const (S.IntConst -1)) -> sprintf "%O[%O]" (modelToStr m) (exprToStr e1) // case where index upper limit is not explicit but inferred
     | S.MIndexed(m,e1,e2) -> 
        sprintf "%O[%O<%O]" (modelToStr m) (exprToStr e1) (exprToStr e2)
     | S.MCall(f,args) -> sprintf "%O(%O)" f (fldsToStr args)
     | S.MRegn r -> "~ "+(RtoString r)
     | S.TypedModel(m,_) -> (modelToStr m) 

  and PredictorToString p = 
     match p with
     | S.Scalar f -> sprintf "%A" f
     | S.Variable (v,_,_) -> ident v
     | S.Interaction (p1,p2) -> sprintf "%O:%O" (PredictorToString p1) (PredictorToString p2)
     | S.Path ([p1],p2) -> sprintf "%O.%O" (PredictorToString p1) (PredictorToString p2)
     | S.Path (ps,p) -> sprintf "(%O).%O" (String.concat "," (List.map PredictorToString ps)) (PredictorToString p) 
     | S.TypedPredictor(p,_,_)-> PredictorToString p
     
  and RtoString r =
     match r with
     | S.Immed p -> sprintf "'%O" (PredictorToString p)
     | S.Sum (r1,r2) -> sprintf "%O + %O" (RtoString r1) (RtoString r2)
     | S.Coeff(p,alpha,r) ->  sprintf "%O{%O~%O}" (PredictorToString p) alpha (RtoString r) 
     | S.Cond(r,p,_) ->  sprintf "(%O|%O)" (RtoString r) (PredictorToString p)
     | S.Noise(d,ps) ->sprintf "%O(%O)" (distToStr d) (String.concat "," (List.map PredictorToString ps))
     | S.Res(v,r) -> sprintf "(new %O)(%O)" v (RtoString r)
      
  and distToStr d =
      match d with
      |  S.GaussianFromMeanAndVariance -> "Gaussian"
      |  S.GammaFromShapeAndScale -> "Gamma"
      |  d -> sprintf "%A" d
  and exprToStr (e:S.Exp) =
   let nestedExprToStr e = 
      if isAtomic e 
      then exprToStr e
      else sprintf "(%O)" (exprToStr e)

   // todo: exploit precedences
   match e with
   | S.Var v -> ident v 
   | S.Const (S.IntConst v) -> sprintf "%A" v
   | S.Const (S.BoolConst v) -> sprintf "%A" v
   | S.Const (S.RealConst v) -> sprintf "%A" v
   | S.Const (S.StringConst v) -> sprintf "%A" v 
   | S.Prim (S.Negate,[e]) -> sprintf "-%O" (nestedExprToStr e)
   | S.Prim (S.Not, [e]) -> sprintf "!%O" (nestedExprToStr e)
   | S.Prim(S.Plus,[e1;e2]) -> sprintf "%O + %O" (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Minus,[e1;e2]) -> sprintf "%O - %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Mult,[e1;e2]) -> sprintf "%O * %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Div,[e1;e2]) -> sprintf "%O / %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Max,[e1;e2]) ->  sprintf "max(%O,%O)"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Mod,[e1;e2]) ->   sprintf "mod(%O,%O)"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Or,[e1;e2]) ->  sprintf "%O | %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.And,[e1;e2]) ->  sprintf "%O & %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Eq,[e1;e2]) -> sprintf "%O = %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Neq,[e1;e2]) -> sprintf "%O != %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Lt,[e1;e2]) -> sprintf "%O < %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Gt,[e1;e2]) -> sprintf "%O > %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.LtEq,[e1;e2]) -> sprintf "%O <= %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.GtEq,[e1;e2]) -> sprintf "%O >= %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Factor(S.FactorName p),es) ->  sprintf "%O(%O)" p (expsToStr es)
   | S.Dist(S.GaussianFromMeanAndVariance,es) -> sprintf "Gaussian(%O)" (expsToStr es)
   | S.Dist(d,es) -> sprintf "%O(%O)" (distToStr d) (expsToStr es)
   | S.SizeOf(t) -> sprintf "SizeOf(%O)" (ident t)
   | S.DeRef(e1,_,cn) -> sprintf "%O.%O" (nestedExprToStr e1) (ident cn) // suppress link
   | S.Ref(tn,cn) -> sprintf "%O.%O"  (ident tn) (ident cn)
   | S.If(e1,e2,e3) -> sprintf "if %O then %O else %O" (exprToStr e1) (exprToStr e2) (exprToStr e3)
   | S.ForLoop(x,e1,e2) -> sprintf "[for %O < %O ->  %O]" (ident x) (nestedExprToStr e1) (exprToStr e2)  
   | S.Array(es) -> sprintf "[ %O ]"  (expsToStr es)
   | S.Subscript(e1,e2) -> sprintf "%O.[%O]" (nestedExprToStr  e1) (exprToStr ( e2))
   | S.Constraint(e1,t1) -> sprintf "%O : %O" (nestedExprToStr e1) (columnTypeToStr t1)
   | S.Let(x,e1,e2) -> sprintf "let %O = %O in %O" (ident x) (exprToStr e1) (exprToStr ( e2))
   | S.Scan(s,x,e1,e2,e3) -> sprintf "Scan((%O,%O)->%O,%O,%O)" s (ident x) (nestedExprToStr e1) (nestedExprToStr e2) (nestedExprToStr e3)
   | S.Infer(d,[],x,e) -> sprintf "infer.%O.%O(%O)" (distToStr d)  (ident x) (exprToStr e) 
   | S.Infer(d,es,x,e) -> sprintf "infer.%O[%O].%O(%O)" (distToStr d)  (expsToStr es) (ident x) (exprToStr e) 
   | S.TypedExp(e,ty) -> exprToStr e
   | _ -> sprintf "?%A" e

  and fldsToStr es = 
      match es with 
      | [] -> ""
      | [(f,e)] -> sprintf "%O=%O" (ident f) (exprToStr ( e))
      | (f,e)::es -> sprintf "%O=%O,%O" (ident f) (exprToStr ( e)) (fldsToStr es)
  and expsToStr es = 
      match es with 
      | [] -> ""
      | [e] -> exprToStr e
      | e::es -> sprintf "%O,%O" (exprToStr ( e)) (expsToStr es)
  and recordTyToStr ts = 
      match ts with 
      | [] -> ""
      | [(f,t)] -> sprintf "%O:%O" (ident f) (columnTypeToStr ( t))
      | (f,t)::ts -> sprintf "%O:%O;%O" (ident f) (columnTypeToStr ( t)) (recordTyToStr ts)
  and columnTypeToStr ty =
     let rec tyToStr ty = 
      match ty with
      | S.T_Real -> "real"
      | S.T_Int -> "int"
      | S.T_Bool -> "bool"
      | S.T_String -> "string"
      | S.T_Link t -> sprintf "link(%O)" (ident t)
      | S.T_Array (ty,e) -> sprintf "%O[%O]" (tyToStr ty) (exprToStr e)
      | S.T_Upto e -> if !uptoAsMod then sprintf "mod(%O)"  (exprToStr e)
                                    else sprintf "upto(%O)" (exprToStr e)
      | S.T_Record flds -> sprintf "{%O}" (recordTyToStr flds)
      | S.T_Vector  -> match !vectorAsArray with 
                       | None -> "vector"
                       | (Some n) ->  columnTypeToStr (S.T_Array(S.T_Real,S.Const (S.IntConst n)))
      | S.T_PositiveDefiniteMatrix -> "PositiveDefiniteMatrix"
     in
      sprintf "%O ! %O" (tyToStr ty) (detToStr (S.det ty))
  
  let levelToStr level = match level with S.Instance -> "" | S.Static -> "static"
  let visibilityToStr visibility = match visibility with S.In -> "input" | S.Local -> "local" | S.Output true -> "latent" | S.Output false -> "output"   
           
  let markupToStr (A:S.Markup) : string =
    match A with
     | S.Hyper(e) -> sprintf "hyper %O" (exprToStr e)
     | S.Param(M) -> sprintf "param %O" (modelToStr M)
     | S.Input -> "input"
     | S.Latent(M) -> sprintf "latent %O" (modelToStr M)
     | S.Observable(M) -> sprintf "output %O" (modelToStr M)
     | (level,vis,M) -> sprintf "%O %O %O" (levelToStr level) (visibilityToStr vis) (modelToStr M)

  let tableToStr (T:S.Table) : string = 
     (List.map (fun(nme,col:S.Column) -> sprintf "%O: %O %O" (ident nme) (columnTypeToStr col.Type) (markupToStr col.Markup)) T)
    |> String.concat "\n  "
  let idStrategyToStr s = match s with
    | Some (S.FromColumn cn) -> sprintf "[%s]" (ident cn)
    | Some S.FromPosition -> "(*positional*)"
    | None -> "(*none*)"
  let declToStr ((S.Declaration (decl, T)):S.Declaration): string  =
    match decl with
    | S.Table(nme,oId) -> sprintf "table %s%s\n%s" (ident nme) (idStrategyToStr oId)  (tableToStr T)
    | S.Fun(nme) -> sprintf "function %s\n  %s" (ident nme) (tableToStr T)

  let schemaToStr (S:S.Schema) = String.concat "\n " (List.map declToStr S)
    
  let toPositional2DStr (m:S.Schema)  = 
      let head mup =  match mup with 
                       |S.Input -> "input" 
                       |S.Latent m -> "latent"
                       |S.Observable m -> "output"
                       |S.Hyper e -> "hyper"
                       |S.Param m -> "param"
                       | (l,v,m) -> sprintf "%s %s" (levelToStr l) (visibilityToStr v) 
      let tail mup = match  mup with 
                     |S.Input -> "" 
                     |S.Latent m -> modelToStr(m) 
                     |S.Observable m -> modelToStr(m)
                     |S.Hyper e -> exprToStr(e)
                     |S.Param m -> modelToStr(m)
                     | (_,_,m) -> modelToStr(m)
      let flip f a b = f b a

      let swap (a,b)  = (b,a)
      let cons x y = x :: y
      let seqconst a = Seq.initInfinite(fun _ -> a)
      let rec interleave = function |([], ys) -> ys |(xs, []) -> xs |(x::xs, y::ys) -> x :: y :: interleave (xs,ys)

      let lines = m |> List.map(fun (S.Declaration(tid,t)) -> 
                                  let header =  
                                      match tid with
                                      | S.Table(n,idStrategy) ->
                                        ident n + idStrategyToStr idStrategy 
                                      | S.Fun(n) ->
                                        "function: " + ident n 
                                  (header,"","","")
                                  ::(t  |> List.map(fun (cn, cmarkup) -> 
                                                           (ident cn, 
                                                            columnTypeToStr cmarkup.Type,
                                                            head cmarkup.Markup,
                                                            tail cmarkup.Markup))
                                         |> List.fold (flip cons) [] |> List.rev
                                    )
                                 )
                    |> List.fold (flip cons) [] |> List.rev
      
      lines |> Seq.zip  (seqconst ["","","",""]) 
            |> Seq.toList 
            |> List.unzip
            |> (interleave << swap)
            |> List.concat


module SExp =
  module S = Syntax
  type  Sexp = Atom of string | List of Sexp list

  let rec private size (t : Sexp) = 
      match t with 
      | Atom s -> s.Length
      | List ts -> Seq.sumBy size ts + ts.Length + 1

  let  pretty tab prefix t = 
      let rec go prefix t = 
        match t with
        | Atom s -> s
        | List (((Atom "program")::ts))->
          let _break = true      
          let sep = if _break then "\n" + prefix + tab else " "
          String.concat sep (Seq.map (go (prefix + tab)) ts)  
        | List (((Atom ("observe"|"assume"|"predict"|"import")::_) as ts)) ->
          let _break = size t > 50        
          let sep = if _break then "\n" + prefix + tab else " "
          "[" + String.concat sep (Seq.map (go (prefix + tab)) ts) + "]" 
        | List ts ->
          let _break = size t > 50        
          let sep = if _break then "\n" + prefix + tab else " "
          "(" + String.concat sep (Seq.map (go (prefix + tab)) ts) + ")" 
       
      prefix + go prefix t

  let list a b = List (Atom a :: b)
         
  //let ident x = x

  let uptoAsMod = ref true
  let vectorAsArray = ref (None:int option)

  let detToStr d = d.ToString() |> Atom
      
  let rec 
    modelToStr (m:S.Model) : Sexp =
    match m with 
     | S.MExp e -> list "Exp" [exprToStr e]
     | S.MIndexed(m,e1,e2) -> 
        list "Indexed" [modelToStr m;exprToStr e1; exprToStr e2]
     | S.MCall(f,args) -> 
        list "Indexed" (Atom f::(fldsToStr args))
     | S.TypedModel(m,mt) -> list "::" [modelToStr m]
  
  and exprToStr (e:S.Exp) : Sexp =
   match e with
   | S.Var v -> list "Var" [Atom v]
   | S.Const (S.IntConst v) -> sprintf "%A" v |> Atom
   | S.Const (S.BoolConst v) -> sprintf "%A" v |> Atom
   | S.Const (S.RealConst v) -> sprintf "%A" v |> Atom
   | S.Const (S.StringConst v) -> sprintf "\"%A\"" v |> Atom
   | S.Prim (p,es) -> list (sprintf "%A" p) (List.map exprToStr es)
   | S.Dist(d,es) -> list (sprintf "%A" d) (List.map exprToStr es)
   | S.SizeOf(t) -> list "SizeOf" [Atom t]
   | S.DeRef(e1,tn,cn) -> list "DeRef" [exprToStr e1; Atom tn;Atom cn]
   | S.Ref(tn,cn) -> list "Ref" [Atom tn;Atom cn]
   | S.If(e1,e2,e3) -> list "If" [exprToStr e1; exprToStr e2; exprToStr e3]
   | S.ForLoop(x,e1,e2) -> list "For" [Atom x; exprToStr e1; exprToStr e2]
   | S.Array(es) -> list "Array" (List.map exprToStr es)
   | S.Subscript(e1,e2) -> list "Subscript"  [exprToStr e1; exprToStr e2]
   | S.Constraint(e1,t1) -> list "Constraint"  [exprToStr e1; columnTypeToStr t1]
   | S.Let(x,e1,e2) -> list "For" [Atom x; exprToStr e1; exprToStr e2]
   | S.Scan(s,x,e1,e2,e3) -> list "Scan" [Atom s; Atom x; exprToStr e1; exprToStr e2;exprToStr e3]
   | S.Infer(d,es,x,e) -> list "Infer" [(sprintf "%A" d|> Atom); List (List.map exprToStr es); Atom x; exprToStr e]
   | S.TypedExp(e,ty) -> list ":" [exprToStr e;columnTypeToStr ty]
   | _ -> Atom (sprintf "?%A" e)

  and fldsToStr es = 
      match es with 
      | [] -> []
      | (f,e)::es -> (list f [exprToStr e])::(fldsToStr es)
  and expsToStr es = 
      match es with 
      | [] -> []
      | e::es -> (exprToStr e)::(expsToStr es)
  and recordTyToStr ts = 
      match ts with 
      | [] -> []
      | (f,t)::ts -> list f [columnTypeToStr t] :: (recordTyToStr ts)
  and columnTypeToStr ty =
     let Base s = List [s;detToStr (S.det ty)] 
     match ty with
      | S.T_Real -> Base (Atom "Real")
      | S.T_Int -> Base (Atom "Int")
      | S.T_Bool -> Base (Atom "Bool")
      | S.T_String -> Base (Atom "String")
      | S.T_Link t -> Base (list "link" [Atom t])
      | S.T_Array (ty,e) -> list "Array" [columnTypeToStr ty;exprToStr e]
      | S.T_Upto e -> Base (list "Upto" [exprToStr e])
      | S.T_Record flds ->  list "Record" (recordTyToStr flds)
      | S.T_Vector  -> Base (Atom "Vector")
      | S.T_PositiveDefiniteMatrix -> Base (Atom "PositiveDefiniteMatrix")

  let markupToStr (A:S.Markup) : Sexp =
    match A with
     | S.Hyper(e) -> list "Hyper" [exprToStr e]
     | S.Param(M) -> list "Param" [modelToStr M]
     | S.Input -> Atom "Input"
     | S.Latent(M) -> list "Latent" [modelToStr M]
     | S.Observable(M) -> list "Output" [modelToStr M]
     | (l,v,M) -> list (Pretty.levelToStr l) [Atom(Pretty.visibilityToStr v); modelToStr M]

  let tableToStr (T:S.Table) : Sexp list = 
      (List.map (fun(nme,col:S.Column) -> 
                list nme [columnTypeToStr col.Type; (markupToStr col.Markup)]) T)
  let declToStr ((S.Declaration (decl, T)):S.Declaration): Sexp  =
    match decl with
    | S.Table(nme,_) -> list "Table" ((Atom nme)::(tableToStr T))
    | S.Fun(nme) -> list "Fun" ((Atom nme)::(tableToStr T))

  let schemaToStr (S:S.Schema) = List (List.map declToStr S)

  let schemaToString S = pretty "  " "" (schemaToStr S)

