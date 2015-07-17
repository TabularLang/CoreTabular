namespace MicrosoftResearch.Infer.Tabular

module Tex =
  module S = MicrosoftResearch.Infer.Tabular.Syntax

  let rec private isAtomic e =
      match e with 
      //TBR this look wrong
      | S.Var _ | S.Const _ | S.SizeOf _  | S.ForLoop _ | S.Array _ | S.DeRef _ | S.Ref _ | S.Prim (S.Factor _,_)| S.Dist _ | S.Subscript _  -> true
      | S.TypedExp(e,t) -> isAtomic e
      | _ -> false
         
  let ident x = x

  let detToStr d = 
      match d with 
      | Syntax.D -> "det"
      | Syntax.R -> "rnd"
      | Syntax.Qry -> "qry"

  let uptoAsMod = ref true

  let rec 
    modelToStr (m:S.Model) : string =
    match m with 
     | S.MExp e -> exprToStr e
     | S.MIndexed(m,e1,e2) -> 
        sprintf "(%O)\\[%O<%O\\]" (modelToStr m) (exprToStr e1) (exprToStr e2)
     | S.MCall(f,args) -> sprintf "%O(%O)" f (fldsToStr args)
     | S.TypedModel(m,_) -> (modelToStr m) 
 
  

  and exprToStr (e:S.Exp) =
   let nestedExprToStr e = 
      if isAtomic e 
      then exprToStr e
      else sprintf "(%O)" (exprToStr e)

   // todo: exploit precedences
   match e with
   | S.Var v -> v 
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
   | S.Prim(S.And,[e1;e2]) ->  sprintf "%O \\& %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Eq,[e1;e2]) -> sprintf "%O = %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Neq,[e1;e2]) -> sprintf "%O != %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Lt,[e1;e2]) -> sprintf "%O < %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Gt,[e1;e2]) -> sprintf "%O > %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.LtEq,[e1;e2]) -> sprintf "%O <= %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.GtEq,[e1;e2]) -> sprintf "%O >= %O"  (nestedExprToStr e1) (nestedExprToStr e2)
   | S.Prim(S.Factor(S.FactorName p),es) ->  sprintf "%O(%O)" p (expsToStr es)
   //| S.Dist(S.GaussianFromMeanAndPrecision,es) -> sprintf "Gaussian(%O)" (expsToStr es)
   //| S.Dist(S.GammaFromShapeAndScale,es) -> sprintf "Gamma(%O)" (expsToStr es)
   | S.Dist(S.GaussianFromMeanAndVariance,es) -> sprintf "Gaussian(%O)" (expsToStr es)
   | S.Dist(d,es) -> sprintf "%A(%O)" d (expsToStr es)
   | S.SizeOf(t) -> sprintf "SizeOf(%O)" t
   | S.DeRef(e1,_,cn) -> sprintf "%O.%O" (nestedExprToStr e1) cn // suppress link
   | S.DeRef(e1,tn,cn) -> sprintf "(%O :> Link(%O)).%O" (nestedExprToStr (e1)) tn cn
   | S.Ref(tn,cn) ->  sprintf "%O.%O" tn cn 
   | S.If(e1,e2,e3) -> sprintf "if %O then %O else %O" (exprToStr e1) (exprToStr e2) (exprToStr e3)
   | S.ForLoop(x,e1,e2) -> sprintf "\\[for %O < %O ->  %O\\]" x (nestedExprToStr e1) (exprToStr e2)  
   | S.Array(es) -> sprintf "\\[%O\\]"  (elemsToStr es)
   | S.Subscript(e1,e2) -> sprintf "%O\\[%O\\]" (nestedExprToStr ( e1)) (exprToStr ( e2))
   | S.Constraint(e1,t1) -> sprintf "%O : %O" (nestedExprToStr ( e1)) (columnTypeToStr ( t1))
   | S.Let(x,e1,e2) -> sprintf "let %O = %O in %O" x (exprToStr ( e1)) (exprToStr ( e2))
   | S.Scan(s,x,e1,e2,e3) -> sprintf "Scan((%O,%O)->%O,%O,%O)" s x (nestedExprToStr ( e1)) (nestedExprToStr ( e2)) (nestedExprToStr ( e3))
   | S.Infer(d,es,x,e) -> sprintf "infer.%A\\[%O\\].%O(%O)" d  (expsToStr es) x (exprToStr e) 
   | S.TypedExp(e,ty) -> exprToStr e
   | _ -> sprintf "?%A" e

  and fldsToStr es = 
      match es with 
      | [] -> ""
      | [(f,e)] -> sprintf "%O=%O" (ident f) (exprToStr ( e))
      | (f,e)::es -> sprintf "%O=%O,%O" (ident f) (exprToStr ( e)) (fldsToStr es)
  and elemsToStr es = 
      match es with 
      | [] -> ""
      | [e] -> exprToStr e
      | e::es -> sprintf "%O;%O" (exprToStr ( e)) (elemsToStr es)
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
     match ty with
      | S.T_Real -> "real"
      | S.T_Int -> "int"
      | S.T_Bool -> "bool"
      | S.T_String -> "string"
      | S.T_Upto(S.TypedExp(S.SizeOf t,_))
      | S.T_Upto(S.SizeOf t)
      | S.T_Link t -> sprintf "link(%O)" t
      | S.T_Array (ty,e) -> sprintf "%O\\[%O\\]" (columnTypeToStr ty) (exprToStr e)
      | S.T_Upto e -> if !uptoAsMod then sprintf "mod(%O)"  (exprToStr e)
                                    else sprintf "upto(%O)" (exprToStr e)
      | S.T_Record flds -> sprintf "{%O}" (recordTyToStr flds)
      | S.T_Vector  -> "vector"
      | S.T_PositiveDefiniteMatrix -> "PositiveDefiniteMatrix"

  let markupToFmt (A:S.Markup) = 
     match A with
     | S.Hyper(e) -> sprintf "\\StaticIn{%s}{%s}\\\\ %%{%s}" //?
     | S.Param(M) -> sprintf "\\StaticOut{%s}{%s}{%s}\\\\"
     | S.Input -> sprintf "\\InstIn{%s}{%s}{%s}\\\\"
     | S.Latent(M) -> sprintf "\\InstOut{%s}{%s}{%s}\\\\"
     | S.Observable(M) -> sprintf "\\InstOut{%s}{%s}{%s}\\\\"
     | (l,v,M) -> sprintf "\\%A%A{%s}{%s}{%s}\\\\" (Pretty.levelToStr l) (Pretty.visibilityToStr v)
  open Syntax



  let TypeToStr ty =
      let d = detToStr (det ty)
      let ty = columnTypeToStr ty
      sprintf "%O!%O" ty d



  let markupToStr (A:S.Markup) : string =
    match A with
     | S.Hyper(e) -> (exprToStr e)
     | S.Param(M) -> (modelToStr M)
     | S.Input -> ""
     | S.Latent(M) -> (modelToStr M)
     | S.Observable(M) -> (modelToStr M)

  let tableToStr (T:S.Table) : string = 
     (List.map (fun(nme,col:S.Column) -> markupToFmt col.Markup nme (TypeToStr col.Type) (markupToStr col.Markup)) T)
    |> String.concat "\n  "
  let declToStr ((S.Declaration (decl, T)):S.Declaration): string  =
    match decl with
    | S.Table(nme,_) -> sprintf "\\TABLE{%s}\\\\\n%s" nme (tableToStr T)
    | S.Fun(nme) -> sprintf "\\FUN{%s}\\\\\n%s"  nme (tableToStr T)

  let schemaToStr (S:S.Schema) = String.concat "\n " ("\\begin{Tabular}"::(List.map declToStr S)@["\\end{Tabular}"])
