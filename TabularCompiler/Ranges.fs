namespace MicrosoftResearch.Infer.Tabular

module Ranges =
  module T = Syntax
  open Target
  type R = RSizeOf of T.TableName | RConst of int
  let ranges = new System.Collections.Generic.Dictionary<R*int,r>()
  let rangeCtxt = ref (fun S -> S)


  let rec rangeOf (R,depth) =
      match R with
      | RConst i ->
        if ranges.ContainsKey((R,depth)) then ranges.[(R,depth)] 
        else 
          if depth <= 0 
          then
             let s = fresh()
             let r = fresh()
             rangeCtxt := 
              (let ctxt = !rangeCtxt 
               fun S -> 
                    ctxt ( Seq(LetVar(s,Const (T.IntConst(i))),
                               Seq(LetRng(r, s),S))))
             ranges.Add((R,depth),r)
             r
          else 
             let r0 = rangeOf (R,0)
             let r = r0+"_"+depth.ToString()
             rangeCtxt := 
              (let ctxt = !rangeCtxt 
               fun S -> 
                    ctxt (Seq(CloneRng(r, r0),S)))
             ranges.Add((R,depth),r)
             r
      | RSizeOf tn ->
        if ranges.ContainsKey((R,depth)) then ranges.[(R,depth)] 
        else 
          if depth <= 0 
          then
             let s = Target.size tn
             let r = Target.range tn
             rangeCtxt := 
              (let ctxt = !rangeCtxt 
               fun S -> 
                    ctxt ( Seq(LetNew(s,T.T_Int), // To be observed
                               Seq(LetRng(r, s),S))))
             ranges.Add((R,depth),r)
             r
          else 
             let r0 = rangeOf (R,0)
             let r = r0+"_"+depth.ToString()
             rangeCtxt := 
              (let ctxt = !rangeCtxt 
               fun S -> 
                    ctxt (Seq(CloneRng(r, r0),S)))
             ranges.Add((R,depth),r)
             r       


  // decRangesXXX - collect all constant ranges via rangeOf
  let rec decRangesExp e = 
     match e with
     | T.Var v -> ()
     | T.Const c -> ()
     | T.Prim (p,es) -> List.iter decRangesExp es
     | T.Dist(d,es) ->  List.iter decRangesExp es
     | T.SizeOf(t) -> ()
     | T.DeRef(e1,tn,cn) ->  decRangesExp e1
     | T.Ref(tn,cn) -> ()
     | T.If(e1,e2,e3) -> decRangesExp e1; decRangesExp e2; decRangesExp e3
     | T.ForLoop(x,e1,e2) -> decRangesExp e1; decRangesExp e2
     | T.Array(es) -> List.iter decRangesExp es
     | T.Subscript(e1,e2) -> decRangesExp e1; decRangesExp e2
     | T.Constraint(e1,t1) -> decRangesExp e1; decRangesColumnType t1
     | T.Let(x,e1,e2) -> decRangesExp e1; decRangesExp e2
     | T.Scan(s,x,e1,e2,e3) -> decRangesExp e1; decRangesExp e2; decRangesExp e3
     | T.Infer(d,es,x,y) ->  List.iter decRangesExp es // shouldn't really occur
     | T.TypedExp(e,t) -> decRangesExp e;decRangesColumnType t
 
  and  decRangesModel m = 
       match m with 
       | T.MExp e -> decRangesExp e
       | T.TypedModel (m,((t1,t2),t3)) -> decRangesModel m; decRangesColumnType t1; decRangesColumnType t2; decRangesColumnType t3
       | _ -> failwithf "decRanges: unexpected non-core model" 

  and depth t =
      match t with
       | T.T_Array (t,e) -> 1+depth t
       | _ -> 0
  and decRangesColumnType t = 
       match t with
       | T.T_Link tn ->  ignore(rangeOf (RSizeOf tn,0))
       | T.T_Real 
       | T.T_Bool 
       | T.T_String
       | T.T_Int 
       | T.T_PositiveDefiniteMatrix-> ()
       | T.T_Upto (T.TypedExp (T.Const (T.IntConst i),_)) -> ignore(rangeOf (RConst i,0))
       | T.T_Upto (T.TypedExp (T.SizeOf tn,_)) -> ignore()
       | T.T_Array (ct,(T.TypedExp (T.Const (T.IntConst i),_))) -> 
           ignore(rangeOf (RConst i,(depth t))); decRangesColumnType ct;
       | T.T_Array (ct,(T.TypedExp (T.SizeOf tn,_))) -> 
           ignore(rangeOf (RSizeOf tn,(depth t))); decRangesColumnType ct;
       | T.T_Array (ct,_) -> 
           decRangesColumnType ct
       | T.T_Record flds -> List.iter (fun (v,ty) -> decRangesColumnType ty) flds
       | T.T_Vector -> ()
  and decRangesMarkup m =
       match m with 
         T.Input -> ()
       | T.Latent m -> decRangesModel m
       | T.Observable m -> decRangesModel m
       | T.Hyper e -> decRangesExp e
       | T.Param m -> decRangesModel m
  let rec decRangesColumns cs = List.iter (fun (cn,col:T.Column) -> decRangesColumnType col.Type;
                                                                    decRangesMarkup col.Markup) cs
  let rec decRangesTable cs = decRangesColumns cs
  let rec decRangesTables decs = 
           match decs with 
           | [] -> ()
           | (T.Declaration(T.Table(tn,_),tbl))::decs ->
             ignore(rangeOf (RSizeOf tn,0));
             decRangesTable tbl;
             decRangesTables decs;
           | dec::decs' -> decRangesTables decs' // skip non-core functions if present
  let rec decRangesSchema decs = decRangesTables decs
