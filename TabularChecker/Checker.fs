module MicrosoftResearch.Infer.Tabular.Checker

open Types
module Tabular = Syntax
open Tabular

open Microsoft.FSharp.Reflection

let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let fromString (t:System.Type) (s:string) =
    match FSharpType.GetUnionCases t |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]))
    |_ -> None


type DistType =    All of VarName * ColumnType * (ExprTyped -> DistType) //dependent
                 | Arr of VarName * ColumnType * DistType // non-dependent
                 | Range of ColumnType
   with override  this.ToString () = 
                  let rec traverse this  : (VarName * ColumnType ) list * ColumnType = 
                    match this with
                    | All(v, T, f)  -> let (xs,r) = traverse (f (Var v))
                                       ((v,T)::xs,r)
                    | Arr(v, T, DT) -> let (xs,r) = traverse DT
                                       ((v,T)::xs,r)
                    | Range(T) ->  ([], T)

                  let revPars,ret = traverse this
                  let s =  (revPars |> List.map (fun (v,T) -> v + ":"  + Pretty.columnTypeToStr T : string))  
                           |> String.concat ","

                  "(" + s + ") -> " + (ret |> Pretty.columnTypeToStr)


let DiscreteType = All ("size" , T_Int , (fun size -> Range (T_Upto size)))
let distTypes =
    let T_Int_R = (T_Det(B_Int,R))
    let T_Real_R = (T_Det(B_Real,R))
    let T_Vector_R = (T_Det(B_Vector,R))
    let T_PDM_R = (T_Det(B_PositiveDefiniteMatrix,R))
    let RxR2R x1 x2 = Arr (x1, T_Real_R, (Arr (x2, T_Real_R, Range(T_Real_R))))
    let IxR2I x1 x2 = Arr (x1, T_Int_R, (Arr (x2, T_Real_R, Range(T_Int_R)))) // (x1:int,x2:real)int
    let VxPDM2V x1 x2 = Arr (x1, T_Vector_R, (Arr (x2, T_PDM_R, Range(T_Vector_R))))
    let RxPDM2PDM x1 x2 = Arr (x1, T_Real_R, (Arr (x2, T_PDM_R, Range(T_PDM_R))))
    [
    //TODO: fill me in
     Beta, RxR2R "alpha" "beta"
     BetaFromMeanAndVariance,RxR2R "mean" "variance"
     GaussianFromMeanAndPrecision, RxR2R "mean" "precision"
     GaussianFromMeanAndVariance, RxR2R "mean" "variance"
     GammaFromShapeAndScale, RxR2R "shape" "scale"
     GammaFromMeanAndVariance,RxR2R "mean" "variance" 
     GammaFromShapeAndRate, RxR2R "shape" "rate"
     Binomial, IxR2I "trialCount" "probSuccess" // this could have a dependent type All(n:int)->float->Upto(n+1)
     VectorGaussianFromMeanAndPrecision, VxPDM2V  "mean" "precision"
     VectorGaussianFromMeanAndVariance, VxPDM2V  "mean" "variance"
     Discrete,  All ("count", T_Int , (fun count -> Arr ("probs", (T_Det(B_Vector (* x *),R)) ,Range (T_Det(B_Upto count,R)))))
     DiscreteUniform, All ("count", T_Int , (fun count -> Range (T_Det(B_Upto count,R))))
     Poisson, Arr ("mean", (T_Det(B_Real,R)), Range (T_Det(B_Int,R)))
     Bernoulli, Arr ("probTrue", (T_Det(B_Real,R)), Range (T_Det(B_Bool,R)))
     Dirichlet, All ("range",T_Int, fun range -> (Arr ("counts", (T_Array(T_Real_R,range)),Range (T_Det(B_Vector (* count*) ,R))))) //TODO: review
     DirichletUniform, All ("count", T_Int, fun count -> Range (T_Det(B_Vector (* count*) ,R)))
     DirichletSymmetric, All ("count", T_Int, fun count -> Arr ("alpha",T_Real_R,(Range (T_Det(B_Vector (* count*) ,R)))))
     WishartFromShapeAndScale, RxPDM2PDM "shape" "scale" 
     WishartFromShapeAndRate, RxPDM2PDM "shape" "rate" 
    ]

let printDistTypes ()  = 
   //let unlines ss = ss |> List.fold (fun (wo,w) e -> let newwo =  w + e in newwo, newwo + "\n") ("","") |> fst 
   distTypes |> List.map (fun (dist,distType) ->   toString dist + ": " + distType.ToString()) 


   
// this is temporary hack since inlining can introduce harmless rebinding.
let rejectShadowing = false

// supT e T d is partial, failing version of total Syntax.supT T d
// e is for error location only
let supT e T d =
      match Syntax.supT T d with 
      | Some T -> T 
      | None -> failwithf "expression %O in space %O cannot be used at type %O in space %O"
                          (Pretty.exprToStr e ) (Pretty.detToStr d)  (Pretty.columnTypeToStr T) (Pretty.detToStr (det T)) 

// supD e d1 dd2 is partial, failing version of total Syntax.supD d1 d2
// e is for error location only
let supD e d1 d2 =
      match supD d1 d2 with 
      | Some d3 -> d3
      | None -> failwithf "in expression %O,  space %O cannot be weakened to space %O"
                          (Pretty.exprToStr e ) (Pretty.detToStr d1 ) (Pretty.detToStr d2)

let rec checkExpr (pc:B) (g:Env) (e:Exp) (t:TargetType) : ExprTyped  =
  let supT = supT e 
  let checkExpr g e t = checkExpr pc g e t
  match e with
  | If (e1, e2, e3) ->
      let (TypedExp(_, t1) as et1) = (checkExpr g e1 T_Bool) 
      //TODO review me:
      let (TypedExp(_, t2) as et2) = (checkExpr g e2 t)
      let (TypedExp(_, t3) as et3) = (checkExpr g e3 t)
      let d1 = det t1
      let d2 = det t2
      let d3 = det t3
      (TypedExp(If (et1, et2, et3), supT t (supD et1 d1 (supD et2 d2 d3)))) //what about det t1?
  | Array es -> 
      match t with
        T_Array (t',f) ->
          //TODO: rewrite me...
          let folder (ls:(ExprTyped list), l:D) (ei:Exp) : ((ExprTyped list)*D) =
            let (TypedExp(_,t'') as eti) = checkExpr g ei t'
            let l' = det t''
            (List.append ls [ eti], supD eti l l') // TBR is this correct?
          if not (areTermsEquivalent g f (TypedExp(Const (IntConst(es.Length)),T_Int)) (T_Int)) 
          then failwithf "expected array of length %O; found array of length %A" (Pretty.exprToStr f) es.Length 
          let (ets,l) = (Seq.fold folder ([  ], D) es)
          (TypedExp(Array ets, supT t l))
       | _ -> failwith "not an array type"
  | Const (IntConst i) -> 
        match t with 
         | T_Int -> 
           TypedExp(e,t)
         | T_Upto (TypedExp(Const(IntConst j),T_Int)) ->
           if not(0 <= i && i < j) then failwithf "constant %O can't have type %O" i (Pretty.columnTypeToStr t)
           TypedExp(e,t)
         | T_Real ->
           // avoid error when Excel auto-formats 0.0 to 0 etc
           let r = System.Convert.ToDouble(i)
           TypedExp(Const (RealConst r),t)
         | _ -> failwithf "cannot type expression %O at type %O" (Pretty.exprToStr e) (Pretty.columnTypeToStr t)
  | Let (x, e, f) ->
        let (TypedExp(_,u) as et) = synthExpr pc g e 
        if rejectShadowing && hasVarOrTable g x then failwith (sprintf "variable %O already in environment" x)
        let (TypedExp(_,v) as ft) = checkExpr (envInsertVar g x (u, pc)) f t
        //TODO: check u is well-formed - if pc is H then it just could mention x!
        TypedExp(Let (x, et, ft), v)
  | Prim(Factor(FactorName f),es) when f.StartsWith("#") -> 
        let ets = List.map (synthExpr pc g) es
        let d = List.fold (fun d (TypedExp(e,t) as et) -> supD et d (det t)) D ets
        let t'  = match f with 
                    | "#Round" -> supT t Qry 
                    | "#Max" -> supT t Qry 
                    | "#Sqrt" -> supT t Qry 
                    | _ -> supT t R
        TypedExp(Prim(Factor(FactorName f),ets),t') (* always random since we don't know qry level semantics; use ##? *)
  | _ ->   
        let (TypedExp(e',t') as et) = synthExpr pc g e
        if (det t <? det t').IsNone // incomparable spaces
        then failwithf "expecting expression in space %O but found expression in space %O" (det t) (det t')
        if not (areTypesEquivalent g t' t) 
        then failwith (sprintf "expecting expression of type %O but found expression of type %O" (Pretty.columnTypeToStr t) (Pretty.columnTypeToStr t'))
        et   
// NB: ignores determinacies
and areTypesEquivalent (g:Env) (t1:TargetType) (t2:TargetType) : bool =
    match (t1, t2) with
      (T_Link tb1, T_Link tb2) -> tb1 = tb2
    | (T_Real, T_Real) -> true
    | (T_Bool, T_Bool) -> true
    | (T_Int, T_Int) -> true
    | (T_Upto e1, T_Upto e2) -> areTermsEquivalent g e1 e2 T_Int 
    | (T_String, T_String) -> true
    | (T_Array (t1, e1), T_Array (t2, e2)) -> areTypesEquivalent g t1 t2 && areTermsEquivalent g e1 e2 T_Int 
    | (T_Record ls1, T_Record ls2) -> //assuming fields in the same order in both records
         let areFieldsEqual = fun elem1 -> fun elem2 ->
                                             let (x1, t1') = elem1
                                             let (x2, t2') = elem2
                                             (x1 = x2) && (areTypesEquivalent g t1' t2')
         List.forall2 areFieldsEqual ls1 ls2
    | (T_Vector,T_Vector) -> true
    | (T_PositiveDefiniteMatrix,T_PositiveDefiniteMatrix) -> true
    | _ -> false

and areTermsEquivalent (g:Env) e1 e2 t: bool =
  match (e1,e2,t) with
  | TypedExp(e1,t1),TypedExp(e2,t2),T_Int->
    match e1, e2 with
    | Const c1, Const c2  -> c1 = c2
    | SizeOf t1,SizeOf t2 -> t1 = t2
    | Var c1,Var c2 -> c1 = c2
    | _ ->
         false


and synthPrim g p ts =
    match p,ts with
    | Negate, [T_Real] -> T_Real
    | Not,[T_Bool] -> T_Bool
    | (Plus|Minus), [T_Int;T_Int] -> T_Int
   // | (Plus|Minus), [ (T_Upto _) as t1;(T_Upto _) as t2]  -> T_Int //?
    | (Plus|Minus|Mult|Div|Max),[T_Real;T_Real] -> T_Real
    | (Lt|Gt|LtEq|GtEq),[T_Real;T_Real] -> T_Bool
    | (Eq|Neq|Lt|Gt|LtEq|GtEq),[T_Int;T_Int] -> T_Bool
   // todo: only allow at qry?
    | (Eq|Neq|Lt|Gt|LtEq|GtEq),[T_String;T_String] -> T_Bool
    | (Eq|Neq|Lt|Gt|LtEq|GtEq),[T_Upto _ as t1;T_Upto e2 as t2] when areTypesEquivalent g t1 t2  -> T_Bool
    | (Eq|Neq|Lt|Gt|LtEq|GtEq),[T_Int; T_Upto _ as t2]  -> T_Bool
    | (Eq|Neq|Lt|Gt|LtEq|GtEq),[T_Upto _ ; T_Int]  -> T_Bool
    | (Eq|Neq|Or|And),[T_Bool;T_Bool] -> T_Bool
    | Factor(FactorName "DampBackward"),[T_Real;T_Real] -> T_Real 
    | Factor(FactorName "Logistic"),[T_Real] -> T_Real
    | Factor(FactorName "Probit"),[T_Real;T_Real] -> T_Bool
    | Factor(FactorName "Sum"),[T_Array(T_Real,_)] -> T_Real
    | Factor(FactorName "Softmax"),[T_Array(T_Real,_)] -> T_Vector
    | Factor(FactorName "DiagonalPDMatrix"),[T_Array(T_Real,_)] -> T_PositiveDefiniteMatrix
    | Factor(FactorName "IdentityScaledBy"),[T_Int;T_Real] -> T_PositiveDefiniteMatrix
     (* TODO: make me dependent! *)
    //NB: interesting types here
    | Factor(FactorName "InnerProduct"),[T_Vector;T_Vector] -> T_Real
    | Factor(FactorName "VectorFromArray"),[T_Array(T_Real,_)] -> T_Vector
    | Factor(FactorName "GetItems"),[T_Array(t,len1);T_Array(T_Det(B_Upto(len1'),D),len2)] 
      when areTermsEquivalent g len1 len1' T_Int ->
       T_Array(t,len2) 
    | Factor(FactorName "Subarray"),[T_Array(t,len1);T_Array(T_Det(B_Upto(len1'),D),len2)] 
      when areTermsEquivalent g len1 len1' T_Int ->
      T_Array(t,len2)
    | Factor(FactorName "Exp"),[T_Real] -> 
      T_Real
    | Factor(FactorName "Log"),[T_Real] -> 
      T_Real
    | Factor(FactorName "BreakSymmetry"),[t] -> 
      t
    | Factor(FactorName "ArgMax"),[T_Array(T_Real,len1)] -> (Types.supT (T_Upto(len1)) Qry).Value
    | Factor(FactorName "ArgMin"),[T_Array(T_Real,len1)] -> (Types.supT (T_Upto(len1)) Qry).Value
    | _ -> failwithf "cannot apply operation %A to arguments of types %A" p (System.String.Join(",",[| for t in ts -> Pretty.columnTypeToStr t |]))

and synthDist pc g (dist,ets) =
    match List.tryFind (fun (dist',dty) -> dist = dist') distTypes with 
    | Some (_,dt) ->
      let rec checkD dt ets =
          match dt,ets with
          | (All (name,t,rangeFn), (TypedExp(e,t') as et::ets)) ->
            let d,d' = det t, det t'
            if areTypesEquivalent g t t'  &&  Option.isSome(d' <=? d)
               then checkD (rangeFn et) ets 
            else failwithf "argument %A of %A: expecting type %O but found type %O" name dist (Pretty.columnTypeToStr t) (Pretty.columnTypeToStr t')
          | (Arr (name,t,range), ((TypedExp(e,t'))::ets)) ->
            let d,d' = det t, det t'
            if areTypesEquivalent g t t' &&  Option.isSome(d' <=? d)
               then checkD range ets 
            else failwithf "argument %A of %A: expecting type %O but found type %O" name dist (Pretty.columnTypeToStr t) (Pretty.columnTypeToStr t')
          | (All (name,_,_) | Arr(name,_,_)),[] -> failwithf "missing argument %A to distribution %A" name dist
          | Range t, [] -> t
          | Range t, (h::_) -> failwithf "extra argument to distribution %A" dist
      checkD dt ets
    | None -> failwithf "NYI: missing type for distribution %A" D 

and synthExpr (pc:B) (g:Env) (e:Exp) : ExprTyped = 
    let supT = supT e
    match e with
    //constants, 
      Const (IntConst c) -> (TypedExp(e, T_Int))
    | Const (RealConst c) -> (TypedExp(e, T_Real))
    | Const (BoolConst c)->  (TypedExp(e, T_Bool))
    | Const (StringConst c) -> (TypedExp(e, T_String))
    //Synth Var
    | Var x ->
        let (t,l) = getType g x
        if (maxB l pc = pc) then
         (TypedExp(Var x, t))
        else failwithf "Variable %A used at level %A is only available at level %A" x pc l
    //Synth Prim (different cases)
    | Prim(p,es) ->
        let ets = List.map (synthExpr pc g) es
        let d = List.fold (fun d (TypedExp(e,t) as et) -> supD et d (det t)) D ets
        let t = synthPrim g p (List.map (fun (TypedExp(e,t)) -> t) ets)
        TypedExp(Prim(p,ets),supT  t d)
    | Dist(dist,es) ->
        let ets = List.map (synthExpr pc g) es // we could also check, not synth, driven by distribution type
        let t = synthDist pc g (dist,ets)
        TypedExp(Dist(dist,ets),supT t R) // always random!  
 
    //Synth Lookup
    | Subscript (e1, e2) ->
        let  et1 = synthExpr pc g e1
        match (extractType et1) with
          T_Array (t,f) -> //return (Index (e1', e2'), t) //V-crusso: todo: check e2 : upto f?
            let (TypedExp(_,t2) as et2) = checkExpr pc g e2 (T_Upto (f))
            (TypedExp(Subscript (et1, et2), supT t (det t2)))
        | _ -> failwithf "cannot index expression of non-array type" 
    // The parser cannot distinguish a Ref from a DeRef so we resolve during typechecking instead.
    | Tabular.DeRef (Var(tb),"", cj) when hasTable g tb -> 
        synthExpr (pc:B) (g:Env) (Tabular.Ref (tb, cj))
    | Tabular.DeRef (e1,tb, cj) -> // TODO: remove ""
        let (TypedExp(_,t) as et1) = synthExpr pc g e1
        match t with
          T_Upto (TypedExp(SizeOf tb',_)) ->
            if (tb<>"" && tb <> tb') then failwith "explicit table %A and inferred table %A don't match in %e" tb tb' (Pretty.exprToStr e)
            let Q = getTableType g tb'
            let (_, _, x, y, z) = Q
            let wholeTable = x @ y @ z
            let tj = lookupFieldType wholeTable cj
            TypedExp(DeRef (et1,tb', cj), supT tj (det t))
        | _ -> failwithf "not a link type"
    | Tabular.Ref (tb, cj) -> 
            let Q = getTableType g tb
            let (h,ws, x, y, z) = Q
            let wholeTable = ws (* don't bind hypers for now, we'd have to substitute out... *)
            let tj = lookupFieldType wholeTable cj
            TypedExp(Ref (tb, cj), tj)
    | SizeOf c ->
           if (hasTable g c) then
             TypedExp(SizeOf c, T_Int)
           else
             failwith (sprintf "table %O not in environment (in SizeOf)" c)
    | ForLoop (x, e, f) ->
        let () =
         match e with 
         | Const(IntConst i) when i >= 0 -> () 
         | SizeOf _ -> ()
         | Var _ -> ()
         | _ -> failwithf "bound of loop must be a constant or SizeOf %O" (Pretty.exprToStr e)  //TODO relax me?
        let (TypedExp(_,t) as et) = checkExpr B.H g e T_Int
        if (rejectShadowing && hasVarOrTable g x) then failwith (sprintf "variable %O already in environment" x)
        if det t > D then failwithf "expected deterministic space for %A; found space %O" x (Pretty.detToStr (det t))
        let (TypedExp(_,u) as ft)= synthExpr pc (envInsertVar g x (T_Upto(et), pc)) f
        TypedExp(ForLoop (x, et, ft), T_Array (u, et))
     | Scan (s,x,e0,e1,e2) ->
        let (TypedExp(_,t2) as et2) = synthExpr pc g e2
        let tx,len = 
            match t2 with 
            | T_Array(tx,len) -> (tx,len) 
            | _ -> failwith (sprintf "third argument of scan must have array type" )
        let (TypedExp(_,t1) as et1)  = synthExpr pc g e1
        if (rejectShadowing && hasVarOrTable g s) then failwith (sprintf "variable %O already in environment" x)
        if (rejectShadowing && hasVarOrTable g x) then failwith (sprintf "variable %O already in environment" x)
        if (x=s) then failwith (sprintf "variables bound by scan must be distinct")
        let gs = envInsertVar g s (t1, pc)
        let gsx = envInsertVar gs x (tx, pc)  
        let (TypedExp(_,u) as et0)= synthExpr pc gsx e0
        TypedExp(Scan (s,x,et0,et1,et2), T_Array (t1, len))
     | Array [] ->
        failwith (sprintf "can't synthesize type of empty array")
     | Array (e0::es) ->
        let (TypedExp(_,t0)) = synthExpr pc g e0
        // should we take a lub of es too?
        checkExpr pc g  e (T_Array(t0,TypedExp(Const(IntConst(1+List.length es)),T_Int)))
     | Let (x, e, f) ->
        let (TypedExp(_,t) as et) = synthExpr pc g e 
        if (rejectShadowing &&  hasVarOrTable g x) then failwith (sprintf "variable %O already in environment" x)
       // if det t > D then failwith (sprintf "expected deterministic bound for %A; found random bound" x)
        let (TypedExp(_,u) as ft)= synthExpr pc (envInsertVar g x (t, pc)) f
        //TODO: check u is well-formed - if pc is H then it just could mention x!
        TypedExp(Let (x, et, ft), supT u (det t))
     | If (e1, e2, e3) -> 
        let (TypedExp(_, t1) as et1) = checkExpr pc g e1 T_Bool 
        //TODO review me:
        let (TypedExp(_, t2) as et2) = synthExpr pc g e2 
        let (TypedExp(_, t3) as et3) = synthExpr pc g e3  
        if not (areTypesEquivalent g t2 t3)  then failwithf "branches have incompatible types %O %O" (Pretty.columnTypeToStr t2) (Pretty.columnTypeToStr t3) 
        let d1 = det t1
        let d2 = det t2
        let d3 = det t3
        (TypedExp(If (et1, et2, et3), supT t2 (supD et1 d1 (supD et2 d2 d3)))) //what about det t1?
     //Constraint
     | Constraint (e1, t1) ->
        let t1' = isWellFormed g t1
        let (TypedExp(_,t1'')as et1)  = checkExpr pc g e1 t1'
        match det t1'' <=? det t1' with
        | Some true -> 
          TypedExp(Constraint (et1, t1'), t1')
        | _ -> failwithf "constrained expression in space %O cannot be in incompatible space %O " (det t1'') (det t1')
     | Infer((Beta|BetaFromMeanAndVariance) as d,[],fld,e1) ->
        let (TypedExp(_, t1) as et1) = checkExpr pc g e1 (T_Det(B_Real,R))
        checkName ["Mean";"mean";"Variance";"variance";"alpha";"trueCount";"beta";"falseCount"] fld
        (TypedExp(Infer(d,[],fld,et1),supT T_Real Qry))
     | Infer(Bernoulli as d,[],fld,e1) ->
        let (TypedExp(_, t1) as et1) = checkExpr pc g e1 (T_Det(B_Bool,R))
        checkName ["Bias";"bias";"Mean";"mean";"probTrue"] fld
        (TypedExp(Infer(d,[],fld,et1),supT T_Real Qry))
     | Infer((GaussianFromMeanAndVariance|GaussianFromMeanAndPrecision) as d,[],fld,e1) ->
        let (TypedExp(_, t1) as et1) = checkExpr pc g e1 (T_Det(B_Real,R))
        checkName ["Mean";"mean";"Variance";"variance";"Precision";"precision";"StdDeviation"] fld
        (TypedExp(Infer(d,[],fld,et1),supT T_Real Qry))
      | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale) as d,[],
              fld,e1) ->
        let (TypedExp(_, t1) as et1) = checkExpr pc g e1 (T_Det(B_Real,R))
        checkName ["Mean";"mean";"Variance";"variance";"Shape";"shape";"Rate";"rate";"Scale";"scale";"StdDeviation"] fld
        (TypedExp(Infer(d,[],fld,et1),supT T_Real Qry))
     | Infer(Dirichlet as d,[e0],fld,e1) ->
        let (TypedExp(_, t1) as et0) = checkExpr pc g e0 (T_Int)
        let (TypedExp(_, t1) as et1) = checkExpr pc g e1 (supT T_Vector R)
        checkName ["Counts";"counts"] fld
        (TypedExp(Infer(d,[et0],fld,et1),T_Array(supT T_Real Qry ,et0)))
     | Infer((Discrete|DiscreteUniform) as d,[e0],fld,e1) ->
        let (TypedExp(_, t1) as et0) = checkExpr pc g e0 (T_Int)
        let (TypedExp(_, t1) as et1) = checkExpr pc g e1 (supT (T_Upto(et0)) R)
        checkName ["Probs";"probs";"Mean";"mean";"Mode";"mode";"Median";"median"] fld
        let t = 
           match fld with
           | ("Probs"|"probs") -> 
              T_Array(supT T_Real Qry ,et0)
           | ("Mode"|"mode"|"Median"|"median") ->
              supT (T_Upto(et0)) Qry
           | ("Mean"|"mean") -> supT T_Real Qry
        in
          TypedExp(Infer(d,[et0],fld,et1),t)
     | TypedExp _ -> failwithf "BUG: unexpected TypedExp %O" (Pretty.exprToStr e)
and checkName ns n = if List.exists (fun n' -> n = n') ns then () else failwithf "expecting distribution property named %A, found %A " ns n 

and isWellFormed (g:Env) (t_in:ColumnType) : TargetType =
    match t_in with
    | Tabular.T_Array (t,e) -> 
        let t' = (isWellFormed g t)
        let (TypedExp(e,u)) as e' = checkExpr H g e T_Int
        if (det u > D) then failwith "array length must be deterministic" else 
        T_Array (t', e')
    | Tabular.T_Det(b,d) ->
        match b with
        | B_Upto(e) ->
          let (TypedExp(e,u)) as e'  = checkExpr H g e T_Int 
          if (det u > D) then failwith "integer bound must be deterministic" else 
          T_Det(B_Upto e',d)
        | B_Link(tb) ->
          if (hasTable g tb) 
          then T_Det(B_Upto (TypedExp(SizeOf tb, T_Int)),d)
          else failwithf "table %O not in environment" tb
        | B_Vector                   // explicit match in case we decide to index
        | B_PositiveDefiniteMatrix   // explicit match in case we decide to index
        | _ -> 
          T_Det(b,d)
    | Tabular.T_Record ls -> //non-dependent
        let mapper = fun pair -> let (name, t) = pair in 
                                 let t' = isWellFormed g t in (name, t')
        T_Record (List.map mapper ls)
    
       
   

