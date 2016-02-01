module MicrosoftResearch.Infer.Tabular.Regression

open Syntax
open Types
open Checker


open Regressions.Semantics   


let R_Real = T_Det(B_Real,D.R)

let GammaOfEnv g = 
        let rec GammaOfEnv G g =  
          match g with
            G_Empty -> Map.empty
          | G_Var ((y, (t,B)), g)-> 
              let G = GammaOfEnv G g
              Map.add y (t,None) G
          | G_Table ((t, (h,w,x,y,z)), g) -> 
              let sizeOf = TypedExp(SizeOf t,T_Int)
              let add G (x,T) = Map. add x (T,Some (Static,t)) G //TBR
              let lift G (x, T)  = Map.add x (T_Array(T,sizeOf),Some (Instance,t)) G
              let G = GammaOfEnv G g
              let G = List.fold add G h
              let G = List.fold add G w
              let G = List.fold lift G x
              let G = List.fold lift G y
              let G = List.fold lift G z
              G
          | G_Model ((y, t), tail) -> 
              GammaOfEnv G tail
        GammaOfEnv Map.empty g
 

// check argument type t is array of form t'[es] (ie nb: t'[en]...[e0] where es = [e0,...,en] )
// returning u where pat t' = Some u.
// NB: passing Some for pat just returns t'. 
let rec (| Arrays|_|) pat es t =
          match es,t with 
          | [],_ -> pat t
          | e::es, T_Array(Arrays pat es.Tail t',e') 
            when areTermsEquivalent G_Empty e e' T_Int -> // it's ok to pass G_Empty, better would be to fix g
            Some t'
          | _,_ -> None

let arrays Q es = 
      List.map (fun (c,ty) -> (c,Arrays ty es)) Q

let rec checkPredictor dim G es e =
        match e with
        | Scalar r -> TypedPredictor(e,Dim dim T_Real, dim <> Id)
        | Variable (c,_,_) -> 
          match Map.tryFind c G with
                   | Some (Arrays (Some) es u,sort) -> 
                     if (Types.det u = Qry) 
                     then failwithf "Predictor %s has unexpected space %s, expecting %s or %s" (Pretty.PredictorToString e) (Pretty.detToStr Qry)  (Pretty.detToStr D.D)  (Pretty.detToStr D.R) 
                     else match u with
                          | Dim dim u' ->
                            TypedPredictor(Variable(c,sort,0),Dim dim u',false) // we annotate with sort for correct elaboration; don't lift
                          | _ ->
                            match dim with
                            | Vector e ->
                              TypedPredictor(Variable(c,sort,0),Dim dim u,true) // need to lift   
                            | _ -> failwithf "Variable %s has type %s, expecting dimensionality %s" (Pretty.PredictorToString e) (Pretty.columnTypeToStr u) (dimToString dim)
                   | Some (u,sort) -> 
                        match u with
                          | Dim dim u' ->
                            TypedPredictor(Variable(c,sort,(List.length es) - (if dim = Id then 0 else 1)),Dim dim u',false) // we annotate with sort for correct elaboration; don't lift
                          | _ ->
                            match dim with
                            | Vector e  ->
                              TypedPredictor(Variable(c,sort,List.length es),Dim dim u,true) // need to lift   
                            | _ -> failwithf "Variable %s has type %s, expecting dimensionality %s" (Pretty.PredictorToString e) (Pretty.columnTypeToStr u) (dimToString dim)
                            // failwithf "Variable %s has type %s, expecting array type with dimensions %s" (Pretty.PredictorToString e) (Pretty.columnTypeToStr t)  (String.concat "" (List.map (fun e -> "["+(Pretty.exprToStr e)+"]") es))
                   | None -> failwithf "Ill-bound variable %s" (Pretty.PredictorToString e)
        | Interaction(p1,p2) ->
          match checkPredictor dim G es p1  ,checkPredictor dim G es p2 with
               | (TypedPredictor(e1,t1,_)) as pt1,(TypedPredictor(e2,t2,_) as pt2) when areTypesEquivalent G_Empty t1 (Dim dim T_Real)  && areTypesEquivalent G_Empty t2 (Dim dim T_Real) ->
                  TypedPredictor(Interaction(pt1,pt2),Dim dim T_Real,false)
               | _ -> failwithf "Ill-typed interaction %s" (Pretty.PredictorToString e)
        | Path(ps,p) ->
          let rec checkPaths ps pts fs  =
               match ps with
               | [] ->
                 match checkPredictor dim G  (List.rev fs) p  with
                 | TypedPredictor(p,t,lift) as pt  -> 
                   TypedPredictor(Path(List.rev pts,pt),t,lift)
               | (pi::ps) ->
                 match checkPredictor dim G es pi with
                 | (TypedPredictor(pi,Dim dim (T_Upto(fi)),li) as pit) ->
                     checkPaths ps (pit::pts) (fi::fs) 
                 | (TypedPredictor(pi,ti,li) as pit) -> failwithf "Ill-typed path component %s; expecting predictor of 'mod(_)' type, found predictor of type %s " (Pretty.PredictorToString pi) (Pretty.columnTypeToStr ti)
          checkPaths ps [] []
        | TypedPredictor _ -> failwith "checkPredictor"


let checkRealPredictor dim G es e =
           match checkPredictor dim G es e with 
           | (TypedPredictor(e,t,_)) as et when areTypesEquivalent G_Empty t (Dim dim T_Real) ->
               et
           | (TypedPredictor(e,t,_)) -> failwithf "expected predictor type %s, found predictor of type %s" (Pretty.columnTypeToStr (Dim dim T_Real))  (Pretty.columnTypeToStr t) 
     
let rec checkRegrn (G:Map<ColumnName,ColumnType* (Level*TableName) option>) es fs (r:Regression) (dim:Dim) : (Regression*RecordType) =
    match r with 
    | Immed e -> 
        let et = checkRealPredictor dim G es e
        (Immed et,[])
    | Cond (r,p,_) ->
        let (TypedPredictor(e,t,_) as pt) = checkPredictor dim G es p
        let f = match t with 
                 | Dim dim (T_Upto(f)) -> f 
                 | _ -> failwithf "attribute %s is expected to be discrete but has type %s" (Pretty.PredictorToString p) (Pretty.columnTypeToStr t)
        let (rt,Pi) = checkRegrn G es (f::fs) r  dim
        (Cond(rt,pt,t),Pi) 
    | Sum(r1,r2) ->
        let (rt1,Q1) = checkRegrn G es fs r1 dim
        let GQ1 = List.fold (fun G (alpha,T) -> Map.add alpha (T,None) G) G Q1
        let (rt2,Q2) = checkRegrn GQ1 es fs r2 dim
        let I = Set.intersect (Set.ofList (List.map fst Q1))  (Set.ofList (List.map fst Q2))
        if not (Set.isEmpty I) then failwithf "duplicate parameter names %s in regression" (String.concat " " (Set.toList I))
        (Sum(rt1,rt2),List.append Q1 Q2 )
    | Coeff(p,alpha,r) ->
        if G.ContainsKey(alpha) then failwithf "Illegal name, variable %s already bound in environment" alpha //TBR
        let et = checkRealPredictor dim G es p
        let (rt,Q) = checkRegrn G fs [] r dim
        if List.exists (fun (c,_) -> c = alpha) Q then failwithf "duplicate parameter name %s in regression" alpha
        (Coeff(et,alpha,rt),arrays ((alpha,Dim dim R_Real)::Q) fs)
    | Noise(d,ps) ->
        let pts = List.map (fun p -> checkPredictor dim G es p) ps
        let ets = List.mapi (fun (i:int) (TypedPredictor(p,Dim dim t,_)) -> TypedExp(Syntax.Var (i.ToString()), t)) pts
        let t = Checker.synthDist W G_Empty (d,ets)
        match t with
        | T_Real -> 
          (Noise(d,pts),[])
        | _ -> failwithf "Noise term %s expected to have type real but has type %s" (Pretty.RtoString r) (Pretty.columnTypeToStr t)
    | Res(v,r) ->
        if G.ContainsKey(v) then failwithf "Illegal restriction, variable %s already bound in environment" v //TBR
        let (rt,Q) = checkRegrn G es fs r dim
        (Res(v,rt),List.filter (fun (pi,t) -> pi<>v) Q)
 
let check g r t = 
        let dim = 
            match t with
            | RealDim dim -> dim
            | _ -> failwithf "regressions must be typed at type %s or %s" (Pretty.columnTypeToStr T_Real) (Pretty.columnTypeToStr (T_Array(T_Real,Syntax.Var "?")))
        checkRegrn (GammaOfEnv g) [] [] r dim 