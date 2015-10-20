module MicrosoftResearch.Infer.Tabular.Model

open Syntax
open Types
open Checker


let rec checkModel (pc:B) (g:Env) (m:Model) (y:TargetType) : ModelTyped =
    match m with
    |  MRegn r ->
       let (rt,Q) = Regression.check g r y
       
       (TypedModel (MRegn rt,
                    ((T_Record Q,T_Record ERT), y)))
    |  MEmpty -> 
        failwith "only inputs may have empty models"
    |  MExp (e) ->
        let (TypedExp(e',y) as et) = checkExpr pc g e y
        (TypedModel (MExp (et), ((T_Record ERT,T_Record ERT), y)))
       // this is for backwards compatibility with OldTabular, which inserts dummy -1 bounds
    |  MIndexed (n, e, Const (IntConst -1)) -> 
        // here, we are ignoring -1 and insert e'' based on the type of e.
        let n' = checkModel pc g n y
        let (TypedModel(_, t)) = n'
        let ((T_Record w,zs),y) = t
        //let (e',l2) = checkExpr g e T_Int //upto?
        //((Indexed (n', e', c), (h, T_Array (w, (Const c, T_Int)), ERT, y)), sup l1 l2)
        let (TypedExp(e',t') as et') = synthExpr pc g e
      //  if det t' > D then failwith (sprintf "expected deterministic bound for %O; found random bound" (Pretty.modelToStr m))
        match t' with
        | T_Upto e'' ->
          (TypedModel(MIndexed (n', et', e''), ((T_Record [ for (wi,wti) in w -> (wi,T_Array (wti, e''))],
                                                 zs),
                                                 y)))
        | _ -> failwithf "index must have upto type"
    |  MIndexed (n, e1, e2) ->
        let n' = checkModel pc g n y
        let (TypedModel(_, t)) = n'
        let ((T_Record w,zs),y) = t
        let (TypedExp(_,t2) as et2) = checkExpr H g e2 T_Int 
        if det t2 > D then failwith (sprintf "expected deterministic bound in %O; found rnd or qry bound" (Pretty.modelToStr m))
        //((Indexed (n', e', c), (h, T_Array (w, (Const c, T_Int)), ERT, y)), sup l1 l2)
        let (TypedExp(_,t1) as et1) = checkExpr pc g e1 (T_Upto(et2)) 
        match t1 with
        | T_Upto et3 ->
          (TypedModel(MIndexed (n', et1, et3), ((T_Record [ for (wi,wti) in w -> (wi,T_Array (wti, et3))],
                                                 zs), 
                                                y)))
        | _ -> failwithf "index must have upto type"
    | MCall(f,es) ->
       let ((rh,rw,rx,[(co,t)],rz),defaults,b) = getModelType g f
      
       let rec checkHypers ets rh rw rx rz defaults es t  =  
           match rh,defaults,es with 
           | [],[],es -> checkInputs ets rw rx rz es t
           | ((n,nt)::rh),(_,_)::ds,(n',e)::es when n = n' -> 
             let et = checkExpr H g e nt
             let S = (e,n)
             let rS = List.map (fun (n',t') -> (n',substT S t'))
             let rh = rS rh
             let rw = rS rw
             let rx = rS rx
             let rz = rS rz
             let t = substT S t
             checkHypers ((n,et)::ets) rh rw rx rz ds es t
           | ((n,nt)::rh),(_,d)::ds,es  -> 
             let et = checkExpr H g d nt
             let S = (d,n)
             let rS = List.map (fun (n',t') -> (n',substT S t'))
             let rh = rS rh
             let rw = rS rw
             let rx = rS rx
             let rz = rS rz
             let t = substT S t
             checkHypers ((n,et)::ets) rh rw rx rz ds es t
       and checkInputs ets rw rx rz es t = 
           match rx,es with 
           | [],[] -> (List.rev ets),((rw,rz),t)
           | [],(n,_)::es -> failwithf "unexpected argument '%O' to function %O \n possible choices %O " n f (Pretty.recordTyToStr (rh@rx))
           | ((n,nt)::rx),(n',e)::es when n = n' -> 
             let et = checkExpr pc g e nt 
             checkInputs ((n,et)::ets) rw rx rz es t 
           | ((n,nt)::rh),es  -> 
             failwithf "call to %O is missing argument %O of type %O" f  n (Pretty.columnTypeToStr nt)
       let (ets,((rw,rz),t)) = checkHypers [] rh rw rx rz defaults es t
       if not (areTypesEquivalent g y t) then
          failwith (sprintf "expecting function with output type %O but found output type %O" (Pretty.columnTypeToStr y) (Pretty.columnTypeToStr t))
       (TypedModel(MCall(f,ets), ((T_Record rw,T_Record rz),t)))

    | _ -> failwithf "NYI: checkModel %A" m 
    
