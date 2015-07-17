namespace MicrosoftResearch.Infer.Tabular

module Erase =
  open Syntax
  let rec 
    model (m:Model)  =
    match m with 
     | MEmpty -> MEmpty
     | MExp e -> MExp(expr e)
     | MIndexed(m,e1,e2) -> 
       MIndexed(model m,expr e1,expr e2)
     | MCall(f,args) -> MCall(f,flds args)
     | TypedModel(m,_) -> model m
 
  
  and expr (e:Exp):Exp =
   match e with
   | Var _  -> e
   | Const _ -> e
   | Prim (p,es) -> Prim(p,List.map expr es) 
   //| S.Dist(S.GaussianFromMeanAndPrecision,es) -> sprintf "Gaussian(%O)" (exps es)
   //| S.Dist(S.GammaFromShapeAndScale,es) -> sprintf "Gamma(%O)" (exps es)
   | Dist(d,es) -> Dist(d,List.map expr es) 
   | SizeOf(t) -> e
   | DeRef(e1,tn,cn) -> DeRef(expr e1,tn,cn)
   | Ref(tn,cn) ->Ref(tn,cn)
   | If(e1,e2,e3) -> If(expr e1,expr e2, expr e3)
   | ForLoop(x,e1,e2) -> ForLoop(x,expr e1,expr e2)
   | Array(es) -> Array (List.map expr es)
   | Subscript(e1,e2) -> Subscript(expr e1,expr e2)
   | Constraint(e1,t1) -> Constraint(expr e1,ty t1)
   | Let(x,e1,e2) -> Let(x,expr e1, expr e2)  
   | Scan(s,x,e1,e2,e3) -> Scan(s,x,expr e1, expr e2, expr e3)
   | Infer(d,es,x,e1) -> Infer(d,List.map expr es,x,expr e1)
   | TypedExp(e,ty) -> expr e
   

  and flds es = List.map (fun (f,e) -> (f,expr e)) es
  
  and recordTy fts = List.map (fun (f,t) -> (f,ty t)) fts
  
  and base_ b = 
      match b with
      | B_Upto(e) -> B_Upto(expr e)
      | b -> b

  and ty t =
     match t with
      | T_Array(t,e) -> T_Array(ty t, expr e)
      | T_Record(ts) -> T_Record (recordTy ts)
      | T_Det(b,d) -> T_Det(base_ b,d)
      
  let markup (level,vis,M) :Markup  = (level,vis,model M)
    
  let table (T:Table)  = 
     (List.map (fun(nme,col:Column) -> (nme,{Type = ty col.Type; Markup= markup col.Markup})) T)
    
  let decl ((Declaration (decl, T)):Declaration)  =
    match decl with
    | Table(nme,oId) -> Declaration(Table(nme,oId),table T)
    | Fun(nme) -> Declaration(Fun(nme),table T)

  let schema (S:Schema) = (List.map decl S)


