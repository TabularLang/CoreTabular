namespace MicrosoftResearch.Infer.Tabular

module Translate  =

  
  module T = Syntax
  open Syntax
  module Tabular = Syntax
  open Target
  open Ranges
  open MicrosoftResearch.Infer
  open MicrosoftResearch.Infer.Factors
  open MicrosoftResearch.Infer.Maths

  let inferNetVersion = typeof<Distributions.Gaussian>.Assembly.GetName().Version


  let rec typeExp TE CE e =
      match e with 
      | TypedExp(e,t) -> t
      | _ -> failwith "BUG: encountered untyped subexpression %A "e
  
  let isConstant e = 
      match e with
      | T.TypedExp((T.Const _),t) -> true
      | _  -> false

  let isConstantArray es = List.forall isConstant es
  let trConstantArray ty es =
      match ty with
      | T_Int -> [| for TypedExp(T.Const (T.IntConst v),_) in es -> v |] :> obj
      | T_Upto(_) -> [| for TypedExp(T.Const (T.IntConst v),_) in es -> v |] :> obj
      | T_Bool ->  [| for TypedExp(T.Const (T.BoolConst v),_) in es -> v |] :> obj
      | T_Real ->  [| for TypedExp(T.Const (T.RealConst v),_) in es -> v |] :> obj
      | T_Link t -> [| for TypedExp(T.Const (T.IntConst v),_) in es -> v |] :> obj
      | T_String  ->  [| for TypedExp(T.Const (T.StringConst v),_) in es -> v |] :> obj
      | _ -> failwith "trConstantArray"


  let rec unArray ty = 
       match  ty with  
        T_Array(ety, (TypedExp(size,_))) ->
          let r' =  match size with 
                    | T.Const(IntConst n)-> rangeOf (RConst n,depth ty) 
                    | T.SizeOf tn -> rangeOf (RSizeOf tn,depth ty) 
                    | _ -> failwith "unarray:1"
          Some (ety,r')
      | T_Array(_,_) -> failwith "unarray:2"
      | ety -> None
  let rec MkArrayAssign_ (av,ety,r) =
      match unArray ety with  
      | Some(ety',r') ->
          let av' = fresh()
          let s' = fresh()
          let v' = fresh()
          let k = MkArrayAssign_(av',ety',s')
          fun E -> 
              Seq(CloneRng(s',r'), 
                  ForEach(s',
                             Seq(LetVar(av',IndexRng(Var av,r)),
                                 Seq(LetVar(v',IndexRng(E,s')),
                                     k (Var v')))))
      | None -> fun E -> Assign(av,r,E)
 

  let MkArrayAssign = 
      if inferNetVersion.Minor <= 5
      then
       MkArrayAssign_
      else
       fun (av,ety,r)  -> fun E -> Assign(av,r,E)
      
       
  
  let rec trLocalExp copyVar tn (TE: Map<TableName,Map<ColumnName,ColumnType>>) (CE:Map<ColumnName,B option * ColumnType>)  et k =
      let compileExp copyVar = trLocalExp copyVar tn TE CE 
      let compileApp copyVar f es k = 
          let rec compileAppAux vs es  =
                    match es with 
                    | [] -> k (f (List.rev vs))
                    | e::es -> compileExp copyVar e 
                                  (fun E -> let v = fresh()
                                            Seq (LetVar(v,E),
                                                  compileAppAux ((Var v)::vs) es))
          compileAppAux [] es
      let adjust copyVar k = 
          if copyVar 
                then
                  let c' = fresh()
                  fun E ->
                  Seq(LetCopy(c',E),
                      k (Var c'))
          else k
      match et with 
      | TypedExp(e,t) ->                 
        match e with
        //TBC
        (*
        | T.Const ((T.IntConst i) as c)->
          (match t with 
             T_Int -> k (Const c )
           | T_Upto(TypedExp(en,_)) ->
              let r = match en with 
                      | T.Const(IntConst n) -> rangeOf n 
                      | T.SizeOf tn -> range(tn)
              let vc = fresh()
              Seq (LetVar(vc,Const c),
                   Seq (SetValueRange(vc,r),
                        k(Var vc)))
          )
          *)
        | T.Const c -> k (Const c)
        | T.SizeOf t-> adjust copyVar k (Var(size t ))
        | T.Prim(T.Factor(T.FactorName("BreakSymmetry")),[e1]) ->
           compileExp copyVar e1 k
        | T.Prim(p,es) ->
          compileApp false (fun vs -> Prim(p,vs)) es k
        | T.Constraint(e1, _) ->
          compileExp copyVar e1 k
        | T.Subscript((TypedExp(_,ta) as ea),(TypedExp(_,ti) as ei)) ->
           match Types.det ti with
           | D ->
              let k = adjust copyVar k
              compileApp false (fun [v1;v2] -> 
                                  Index(v1,v2)) [ea;ei] k
           | R ->  
                let r = match  ta with 
                        | T_Array(_,(TypedExp(T.Const(IntConst n),_))) -> rangeOf (RConst n,depth ta) 
                        | T_Array(_,(TypedExp(T.SizeOf tn,_)))->  rangeOf (RSizeOf tn,depth ta) 
                        | _ -> failwithf "BUG: array type with %A with  non-constant size" ta 
                let ret = fresh()
                let wi = fresh()
                let ve = fresh()
                let cr = fresh()
                compileExp false  ea (fun EA ->
                compileExp false ei (fun EI ->
                Seq(LetNew(ret,t),
                    let v1 = fresh()
                    Seq (LetCopy(ve,EI),
                          Seq (CloneRng(cr,r),
                              Seq (SetValueRange(ve,cr),
                                    Seq(Switch(ve,
                                              Seq (LetVar (wi,Index(EA,Var ve)),
                                                  Seq (SetTo(ret,Var wi),Skip))),
                                        k (Var ret))))))
                ))
           
        // this special-casing is a hack to avoid a bug in Infer.NET 2.5
        | T.Array es when List.forall isConstant es && false ->
            match t with 
            | T_Array(et,_) ->
                let obj = trConstantArray et es
                let r = rangeOf (RConst es.Length,depth t)
                let av = fresh()
                Seq(LetArray(av,r,et),
                    Seq(ObserveValue(av,t,obj),
                        k (Var av)))
        | T.Array es ->
            match t with 
            | T_Array(et,_) ->
                let k = adjust copyVar k
                let r = rangeOf (RConst es.Length, depth t)
                let av = fresh()
                Seq(LetArray(av,r,et),
                     compileApp true (fun vs -> vs) es (fun vs ->
                       let sets = List.mapi (fun i v -> AssignIndex(av,Const (IntConst i),v)) vs
                       List.foldBack (fun s ss -> Seq(s,ss)) sets (k (Var av))))
                       
        // this disabled special-casing is a hack to avoid a bug in Infer.NET 2.5
        |  T.ForLoop(x,TypedExp((T.Const (IntConst n)) as e1 ,_), (TypedExp(_,et) as e2)) when false && isConstant e2  ->
                let k = adjust copyVar k
                let obj = trConstantArray et [ for i in 1..n -> e2]
                let r = rangeOf (RConst n,depth t)
                let av = fresh()
                Seq(LetArray(av,r,et),
                    Seq(ObserveValue(av,t,obj),
                        k (Var av)))
        // this is a special case hack to allow nested one-dimensional array expressions within loops that loop over the same range.
        // this differs from the general case code below in copying the range before iterating over the copy - 
        // See Bugs\RangeBug.xlsx
        | T.ForLoop(x,TypedExp((T.Const (IntConst _) | T.SizeOf _) as e1 ,_),
                      (TypedExp(_,t2) as et2)) when depth t = 1  ->
            let k =  adjust copyVar k
            let r = match e1 with  
                    | T.Const (IntConst i) -> rangeOf (RConst i,depth t)
                    | T.SizeOf tn ->rangeOf (RSizeOf tn, depth t)
                   // | _ -> failwith "impossible"
            let av = fresh()
            let CEx = CE.Add(x,(None,T_Upto(e1)))
            let s = fresh()
            //let x = fresh()
            Seq(LetArray(av,r,t2),//LetArray(av,r,t2)
                Seq(CloneRng(s,r),
                 Seq(ForLoop(s,x,
                             trLocalExp true tn TE CEx et2 (fun E -> Assign(av,s,E))),
                              // trLocalExp true tn TE CEx et2 (fun E -> AssignIndex(av,Var x,E))),
                        k (Var av))))
    (* works)
        | T.ForLoop(x,TypedExp((T.Const (IntConst _) | T.SizeOf _) as e1 ,_),
                      (TypedExp(_,t2) as et2)) ->
            // What is the right code for this?
            let k =  adjust copyVar k
            let r = match e1 with  
                    | T.Const (IntConst i) -> rangeOf i
                    | T.SizeOf tn -> range(tn)
                   // | _ -> failwith "impossible"x
            let s = fresh()
            let av = fresh()
            let CEx = CE.Add(x,(None,T_Upto(e1)))
            let s = r
            //let x = fresh()
            //Seq(CloneRng(s,r), 
            Seq(LetArray(av,s,t2),//LetArray(av,r,t2)
                Seq(ForLoop(s,x,
                            //    trLocalExp true tn TE CEx et2 (fun E -> Assign(av,s,E))),
                            trLocalExp true tn TE CEx et2 (fun E -> AssignIndex(av,Var x,E))),
                    k (Var av)))
            //)
   *)
        | T.ForLoop(x,TypedExp((T.Const (IntConst _) | T.SizeOf _) as e1 ,_),
                      (TypedExp(_,t2) as et2)) ->
            let k =  adjust copyVar k
            let r = match e1 with  
                    | T.Const (IntConst i) -> rangeOf (RConst i,depth t)
                    | T.SizeOf tn ->rangeOf (RSizeOf tn, depth t)
                   // | _ -> failwith "impossible"
            let av = fresh()
            let CEx = CE.Add(x,(None,T_Upto(e1)))
            //let x = fresh()
            Seq(LetArray(av,r,t2),//LetArray(av,r,t2)
                 Seq(ForLoop(r,x,
                             trLocalExp true tn TE CEx et2 (fun E -> Assign(av,r,E))),
                              // trLocalExp true tn TE CEx et2 (fun E -> AssignIndex(av,Var x,E))),
                        k (Var av)))
            //)
   (*
        | T.ForLoop(x,TypedExp((T.Const (IntConst _) | T.SizeOf _) as e1 ,_),
                      (TypedExp(_,t2) as et2)) ->
            // What is the right code for this?
            let k =  adjust copyVar k
            let r = match e1 with  
                    | T.Const (IntConst i) -> rangeOf i
                    | T.SizeOf tn -> range(tn)
                   // | _ -> failwith "impossible"
            let s = fresh()
            let av = fresh()
            let CEx = CE.Add(x,(None,T_Upto(e1)))
            //let x = fresh()
            Seq(LetArray(av,r,t2),
                Seq(CloneRng(s,r), 
                 //LetArray(av,r,t2)
                    Seq(ForLoop(s,x,
                                //trLocalExp true tn TE CEx et2 (fun E -> Assign(av,s,E))),
                                trLocalExp true tn TE CEx et2 (fun E -> AssignIndex(av,Var x,E))),
                        k (Var av)))
            )
    *)
      
         | T.Scan(s,x,
                      (TypedExp(_,t0) as et0),(TypedExp(_,t1) as et1),(TypedExp(_,t2) as et2)) when false  ->
            // What is the right code for this?
            let (tx,r) = match t2 with  
                    | T_Array(tx,TypedExp(T.Const (IntConst i),_)) -> (tx,rangeOf (RConst i,depth t2)) // check depth
                    | T_Array(tx,TypedExp(T.SizeOf(tn),_)) -> (tx,rangeOf (RSizeOf tn,depth t2)) // check depth
                    | _ -> failwith "impossible"
            let rc = fresh()
            let av = fresh()
            let i = fresh()
            let b = fresh()
            let CEs = CE.Add(s,(None,t1))
            let CEsx = CEs.Add(x,(None,tx))
            //let x = fresh()
            //TODO: bind x!
            let avi = fresh()
            let bind Ei Es Ex = Seq(LetVar(s,Es),
                                 Seq(LetVar(x,Ex),
                                     trLocalExp true tn TE CEsx et0 (fun E -> Seq(LetVar(avi,Index(Var av,Ei)),SetTo(avi,E)))))
            //let foo() = trLocalExp tn TE CE (E.Let(s,e0,Let(x,T.Subscript( (fun E -> Assign(av,rc,E))
            compileExp false et1 (fun E1 ->
            compileExp false et2 (fun E2 ->
              Seq(CloneRng(rc,r), 
                Seq(LetArray(av,rc,t0),
                    Seq(ForLoop(rc,i,
                                Seq(LetVar(b,Prim(Tabular.Eq,[Var i;Const (IntConst 0)])),  
                                    Seq(If(b, 
                                           bind (Var i)  E1 (Index(E2,Var i))),
                                        Seq(IfNot(b, 
                                                  bind (Var i) (Index(Var av,Prim(Tabular.Minus,[Var i;Const (IntConst 1)])))
                                                       (Index(E2,Var i))),
                                            Skip)))
                               ),
                        k (Var av))))))
        (*
            let bind Es Ex = Seq(LetVar(s,Es),
                                 Seq(LetVar(x,Ex),
                                     trLocalExp tn TE CEsx et0 (fun E -> Assign(av,rc,E))))
            //let foo() = trLocalExp tn TE CE (E.Let(s,e0,Let(x,T.Subscript( (fun E -> Assign(av,rc,E))
            compileExp et1 (fun E1 ->
            compileExp et2 (fun E2 ->
              Seq(CloneRng(rc,r), 
                Seq(LetArray(av,rc,t0),
                    Seq(ForLoop(rc,i,
                                Seq(LetVar(b,Prim(Tabular.Eq,[Var i;Const 0])),  
                                    Seq(If(b, 
                                           bind E1 (Index(E2,Var i))),
                                        Seq(IfNot(b, 
                                                  bind (Index(Var av,Prim(Tabular.Minus,[Var i;Const 1])))
                                                       (Index(E2,Var i))),
                                            Skip)))
                               ),
                         k (Var av))))))
        *)

        | T.Var c -> 
          let k = adjust copyVar k 
          match CE.[c] with
           | (Some B.H,_) 
           | (Some B.W,_) -> k (Var(col(tn,c)))
           | (Some B.Y,_) -> k (IndexRng  (Var(col(tn,c)),range tn )) 
           | (None,_) -> k(Var c) //for let and for bound variables (not in CE)
        (*
        | T.Var c when (not copyVar) -> //when CE.ContainsKey(c) -> 
        | T.Var c when copyVar -> 
           let c' = fresh()
           match CE.[c] with
           | (Some B.H,_) 
           | (Some B.W,_) -> Seq(LetCopy(c',Var(col(tn,c))),
                                 k (Var c'))
           | (Some B.Y,_) ->  Seq(LetCopy(c',IndexRng  (Var(col(tn,c)),range tn)),
                                  k(Var c'))
           | (None,_) -> Seq(LetCopy(c',Var c),
                                  k(Var c')) //for let and for bound variables (not in CE)
         *)
        | T.DeRef (TypedExp(_,t1) as e1,tn',d) when Types.det t1 = D->
            let k = adjust copyVar k 
            match t1 with 
            | T_Upto (TypedExp (SizeOf tn,_)) 
            | T_Link tn -> //note we could rely on T_Link tn being translated to T_Upto (SizeOf tn?)
              assert(tn=tn')
              let tn'd = col(tn',d)
              compileApp false (fun [v1] -> Index(Var(tn'd),v1)) [e1] k
            | _ -> failwithf "BUG: compiling %A" e
        | T.DeRef (TypedExp(_,t1) as e1,tn',d) when Types.det t1 = R-> 
            match t1 with 
            | T_Upto (TypedExp (SizeOf tn,_)) 
            | T_Link tn -> //note we could rely on T_Link tn being translated to T_Upto (SizeOf tn?)
              assert(tn=tn')
              let tn'd = col(tn',d)
              let r =  range(tn)
              let ret = fresh()
              let cr = fresh()
              let ve = fresh()
              let wi = fresh()
              compileExp false e1 (fun E1 ->
                    Seq(LetNew(ret,t),
                        Seq (LetCopy(ve,E1),
                             Seq (CloneRng(cr,r),
                                  Seq (SetValueRange(ve,cr),
                                       Seq(Switch(ve,
                                                 Seq (//LetVar(wi,Index(Var(tn'd),Var ve)), 
                                                      LetCopy(wi,Index(Var(tn'd),Var ve)), // best to copy?
                                                      Seq (SetTo(ret,Var wi),Skip))),
                                           k (Var ret))))))
                    )
             // compileApp false (fun [v1] -> Index(Var(tn'd),v1)) [e1] k
            | _ -> failwithf "BUG: compiling %A" e
        | T.Ref (tn,c) ->
           (*
           if copyVar 
           then //TBR
             let c' = fresh()
             Seq(LetCopy(c',Var(col(tn,c))),
                         k (Var c'))
           else
           *)
           let k = adjust copyVar k
           k (Var(col(tn,c)))
        | T.If(e1,e2,e3) ->
           compileExp false e1 
                  (fun E1 ->
                       let ret = fresh()
                       Seq (LetNew(ret,t),
                            let v1 = fresh()
                            Seq(LetVar(v1,E1),  
                                Seq(If(v1, 
                                       (compileExp true e2 (fun E2 -> Seq(SetTo(ret,E2),Skip)))),
                                    Seq(IfNot(v1, 
                                              (compileExp true e3 (fun E3 -> Seq(SetTo(ret,E3),Skip)))),
                                        k (Var ret))))))
        | T.Let(x,(TypedExp(_,t1) as e1),e2) ->
           let CEx = CE.Add(x,(None,t1))
           compileExp false e1 
                  (fun E1 ->
                       Seq(LetVar(x,E1),trLocalExp copyVar tn TE CEx e2 k))  
(* old symmetry breaking      
        | T.Prim(T.Factor(T.FactorName("BreakSymmetry")),[T.TypedExp(T.Dist(d,es),_)])    
        | T.Dist(d,es) -> 
           let breakSymmetry = match e with T.Prim _ -> true | _ -> false
           //TODO: use dep. type of d to drive range/exp interpretation of es
           match d,es with
           | (Bernoulli,[TypedExp(e0,_)]) ->
             match e0 with
             | T.Const(T.RealConst(p)) when breakSymmetry ->
               let b = Rand.Double() < p
               let o = Distributions.Bernoulli.PointMass(b)
               compileApp false (fun vs ->InitialiseTo(Dist(d,vs),o)) es k 
             | T.Dist(Beta,[TypedExp(T.Const(T.RealConst(a)),_);TypedExp(T.Const(T.RealConst(b)),_)]) when breakSymmetry ->
               let p = a / (a+b)
               let b = Rand.Double() < p
               let o = Distributions.Bernoulli.PointMass(b)
               compileApp false (fun vs ->InitialiseTo(Dist(d,vs),o)) es k 
             | _ -> 
                compileApp false (fun vs -> Dist(d,vs)) es k 
           | (Dirichlet | DirichletUniform |DirichletSymmetric), ((TypedExp(e0,_))::es) ->
             match e0 with 
             | T.Const(IntConst n) when breakSymmetry ->
                let r = rangeOf n 
                let o = dirichletInit(n)
                compileApp false (fun vs ->InitialiseTo(Dist(d,(Rng r) ::vs),o)) es k 
             | _ ->
             let r = match e0 with 
                     | T.Const(IntConst n) -> rangeOf n 
                     | T.SizeOf(tn) -> range(tn)
                     | _ -> failwithf "BUG: %A has non-constant argument %A" d e0 
             compileApp false (fun vs -> Dist(d,(Rng r) ::vs)) es k 
           | Discrete, ((TypedExp(e0,_))::es) ->
             match e0 with 
             | T.Const(IntConst n) when breakSymmetry ->
               // add symmetry breaking
               let r = rangeOf n
               let v = Distributions.Dirichlet([| for i in 1 .. n -> 10.0 |]).Sample()
               let o = Distributions.Discrete(v)
               //let o = Distributions.Discrete.PointMass(Rand.Int(n),n) :> obj
               compileApp false (fun vs -> InitialiseTo(Dist(d,(Rng r) ::vs),o)) es k
             | _ -> 
               let r = match e0 with 
                     | T.Const(IntConst n) -> rangeOf n 
                     | T.SizeOf(tn) -> range(tn)
                     | _ -> failwithf "BUG: %A has non-constant argument %A" d e0 
               compileApp false (fun vs -> Dist(d,(Rng r) ::vs)) es k   
             | _ -> failwithf "BUG: %A has non-constant argument %A" d e0 
           | _,_ ->
           compileApp false (fun vs -> Dist(d,vs)) es k 
  *)
        | T.Dist(d,es) -> 
           //TODO: use dep. type of d to drive range/exp interpretation of es
           match d,es with
           | (Dirichlet | DirichletUniform |DirichletSymmetric), ((TypedExp(e0,_))::es) ->
             let r = match e0 with 
                     | T.Const(IntConst n) -> rangeOf (RConst n,0)
                     | T.SizeOf(tn) -> rangeOf (RSizeOf tn,0)
                     | _ -> failwithf "BUG: %A has non-constant argument %A" d e0 
             compileApp false (fun vs -> Dist(d,(Rng r) ::vs)) es k 
           | Discrete, ((TypedExp(e0,_))::es) ->
               let r = match e0 with 
                       | T.Const(IntConst n) ->rangeOf (RConst n,0) 
                       | T.SizeOf(tn) ->  rangeOf (RSizeOf tn,0)
                       | _ -> failwithf "BUG: %A has non-constant argument %A" d e0 
               compileApp false (fun vs -> Dist(d,(Rng r) ::vs)) es k   
           | _,_ ->
           compileApp false (fun vs -> Dist(d,vs)) es k 
      | ue -> failwithf "BUG: encountered untyped expression %A " ue
 
  let trExp copyVar tn (TE: Map<TableName,Map<ColumnName,ColumnType>>) (CE:Map<ColumnName,B*ColumnType>)  et =
      // lift CE to allow local variables sans binding times 
      trLocalExp copyVar tn TE (Map.map (fun k (B,T) -> (Some B,T)) CE) et
 
  type Es = E list
  type KE = (E -> S) -> S
  type KEs = (Es -> S) -> S

  // TODO: simplify MT (legacy)
  type MT = {TH: ColumnType list;TW: ColumnType list; TX: ColumnType; TY: ColumnType}
  

  
  

  let rec trModel tn TE CE  mt : KE =
      match mt with 
      | TypedModel(m,t) ->
       match m with 
       | MExp(e) ->  trExp true tn TE CE e
       | _ -> failwith "trModel: unexpected model %A" m
     

  type ColumnInfo = {B:B}
  let rec trTables
            (TI: Map<TableName,Map<ColumnName,ColumnInfo>>)
            (TE: Map<TableName,Map<ColumnName,ColumnType>>)
            tables = 
      match tables with
      | [] -> (TI,TE,Skip)
      | (Declaration(Table(tn,_),table)::tables) -> 
        let s = size tn
        let rn = range(tn)
        let rec trColumns CI CE columns  =
          match columns with
          |  [] -> 
            trTables (TI.Add(tn,CI)) (TE.Add(tn,Map.map  (fun col (b,ty) -> ty)  CE)) tables
          | (cn,{Type=ty;Markup=m})::rest ->
            if Types.det ty = Qry
            then trColumns CI CE rest // skip queries
            else
            let cv =  col(tn,cn)
            let r = range(tn)
            match m with
            | Hyper e ->
               let CE' = CE.Add(cn,(H,ty))
               let (TI,TE,S) = trColumns CI CE' rest
               (TI,TE,
                trExp false tn TE CE e (fun E -> Seq(LetVar(cv,E),S)))//TBD
            | Param m ->
              let CE' = CE.Add(cn,(W,ty))
              let m = trModel tn TE CE m
              let CI' = CI.Add(cn,{B=W})
              //let (TI,TE,S) = trColumns CI' CE' rest
              //(TI,TE, m (fun E -> Seq(LetVar(cv,E),S)))  
              let S1 = m (fun E -> LetVar(cv,E))
              let (TI,TE,S2) = trColumns CI' CE' rest
              (TI,TE,Seq (S1,S2))
            | Input ->
               let CE' = CE.Add(cn,(Y,ty))
               let (TI,TE,S) = trColumns CI CE' rest
               (TI,TE,
                Seq(LetArray(cv,r,ty), //TBC
                    S))

            // this is a special cased compilation of Scan that works with TrueSkill through time...
            // but doesn't work for RobotLatent.xlsx (which works fine otherwise) 
            // disabled for now using "when false"
            // | Observable (TypedModel(MExp(TypedExp(T.Scan(_,_,_,_,_) as e,_)),_)) 
            | Latent  (TypedModel(MExp(TypedExp(T.Scan(_,_,_,_,_)as e,_)),_)) when false ->
              let CE' = CE.Add(cn,(Y,ty))
              let CI' = CI.Add(cn,({B=Y}))
              match e with 
                | T.Scan(s,x,(TypedExp(_,t0) as et0),(TypedExp(_,t1) as et1),(TypedExp(_,t2) as et2)) ->
                let (tx,r2) = match t2 with  
                        | T_Array(tx,TypedExp(T.Const (IntConst i),_)) -> (tx,rangeOf (RConst i,depth t2))
                        | T_Array(tx,TypedExp(T.SizeOf(tn),_)) -> (tx,rangeOf (RSizeOf tn,depth t2))
                        | _ -> failwith "impossible"
                let rc = fresh()
                let av = fresh()
                let k = fresh()
                let i = fresh()
                let b = fresh()
                let CE = (Map.map (fun k (B,T) -> (Some B,T)) CE)
                let compileExp copyVar = trLocalExp copyVar tn TE CE 
                let CEs = CE.Add(s,(None,t1))
                let CEsx = CEs.Add(x,(None,tx))
                //let x = fresh()
                //TODO: bind x!
                let avi = fresh()
                let bind Ei Es Ex = Seq(LetVar(s,Es),
                                     Seq(LetVar(x,Ex),
                                         trLocalExp true tn TE CEsx et0 (fun E -> Seq(LetVar(avi,Index(Var av,Ei)),SetTo(avi,E)))))
                let (TI,TE,S) = trColumns CI' CE' rest
                (TI,TE,
                   Seq(LetArray(cv,r,ty),
                       Seq(ForLoop(r,k,
                                   compileExp false et1 (fun E1 ->
                                   compileExp false et2 (fun E2 ->
                                   Seq(CloneRng(rc,r2), 
                                   Seq(LetVar(av,Index(Var cv,Var k)),
                                   Seq(ForLoop(rc,i,
                                               Seq(LetVar(b,Prim(Tabular.Eq,[Var i;Const (IntConst 0)])),  
                                                   Seq(If(b, 
                                                          bind (Var i)  E1 (Index(E2,Var i))),
                                                       Seq(IfNot(b, 
                                                                 bind (Var i) (Index(Var av,Prim(Tabular.Minus,[Var i;Const (IntConst 1)])))
                                                                       (Index(E2,Var i))),
                                            Skip)))
                                   ),
                                   Skip)))))
                                   ),
                            S)))
            | Observable m ->
              let seq l = List.foldBack (fun s ss -> Seq (s,ss)) l Skip
              let CE' = CE.Add(cn,(Y,ty))
              let m = trModel tn TE CE m
              let CI' = CI.Add(cn,({B=Y}))
              let body = m (MkArrayAssign(cv,ty,r))
              let sv = subarray(tn,cn)
              let ss = subarraysize(tn,cn)
              let sr = subarrayrange(tn,cn)
              let si = subarrayindices(tn,cn)
              let (TI,TE,S) = trColumns CI' CE' rest
              (TI,TE, Seq (seq [LetArray(cv,r,ty)
                                ForEach(r,body)
                                LetNew(ss,T_Int)
                                LetRng(sr,ss)
                                LetArray(si,sr,T_Upto(TypedExp(SizeOf(tn),T_Int))) // is the size necessary?
                                LetVar(sv,E.Prim(Prim.Factor(FactorName "Subarray"),[Var cv;Var si]))
                               ],
                             S))
            | Latent m -> 
              let CE' = CE.Add(cn,(Y,ty))
              let m = trModel tn TE CE m
              let CI' = CI.Add(cn,({B=Y}))
              let body = m (MkArrayAssign(cv,ty,r))
              let (TI,TE,S) = trColumns CI' CE' rest
              (TI,TE, Seq (LetArray(cv,r,ty),
                              Seq(ForEach(r,
                                          body),
                                  S)))
        let (TI,TE,S) = trColumns Map.empty Map.empty table
        (TI,TE,S)

  and trSchemaWithInfo(schema:Schema) =
      // reset state
      i <- 0
      ranges.Clear()
      rangeCtxt := fun S -> S
    //  MicrosoftResearch.Infer.Maths.Rand.Restart(123567)
      decRangesSchema schema
      let (TI,TE,S) =  (trTables Map.empty Map.empty schema)
      (TI,TE,(!rangeCtxt) S)
  and trSchema(schema:Schema) =
      let (TI,TE,S) = trSchemaWithInfo(schema:Schema) 
      S


