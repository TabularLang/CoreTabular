module MicrosoftResearch.Infer.Tabular.Table

module Tabular = Syntax
open Tabular

open Types
open Checker
open Model

type LogValue = ModelType of ModelType  | Err of string
type Log = Map<ColumnName,LogValue>

let levelOf level = match level with Instance -> Y | Static -> W


let rec synthTable isTable (g:Env) (tb:Table) : Log * Error * (Table * TableType) =
    let qualify m c w = match m with MRegn n -> w | _ -> qualify c w 
    match tb with
      [] -> (Map.empty, false, ([], (ERT, ERT, ERT, ERT, ERT)))
    | hd :: tl -> 
        let hd = Regressions.Sugar.desugar hd // desugar any regression
        let (c, col) = hd
        let tc = isWellFormed g col.Type 
        let markup : Markup = col.Markup
        let (level,visibility,M) = markup
        if (hasVarOrTable g c) then failwith (sprintf "variable %O already in environment" c)
        match markup with 
        | Tabular.Hyper (e) ->
            if isTable && det tc <> D  then failwith (sprintf "hyper %O must be declared deterministic, but is not"  c) 
            let tb1:Table = tl
            let (logValue,err,e') =
              try 
                let (TypedExp(e',tc') as et') = checkExpr H g e tc
                if isTable && det tc' > D then failwith (sprintf "hyper %O must be deterministic, but is random"  c) 
                (ModelType ((T_Record ERT,T_Record ERT),T_Record ERT),false,et') 
              with Failure s ->
                let bogus = TypedExp(Tabular.Var "bogus",tc)
                (Err s,true,bogus)
            let (TypedExp(_, tc)) = e'
            let g1 = envInsertVar g c (tc,H)
            let (log',err',((tb1,ty1) as tb1'ty)) = synthTable isTable g1 tb1 
            let tb1'ty  = if isTable 
                          // substitute value after binding -- this is why we require det tc when isTable
                          then let e'c = (e',c)
                               List.map (fun (cn,col) -> (cn, substC (e',c) col)) tb1,
                                let (h,w,x,y,z) = ty1
                                (substRT e'c h, substRT e'c w, substRT e'c x, substRT e'c y, substRT e'c z)
                          else tb1'ty
            let log = log'.Add(c,logValue)
            let (tb1, ty) = tb1'ty
            let (h, w, x, y, z) = ty
            let h1 = (c, tc) :: h
            let firstCol = {Type = tc; Markup = Hyper e'}
            (log,err||err',(((c, firstCol) :: tb1), (h1, w, x, y, z)))
        | Tabular.Param m ->
            let tb1:Table = tl
            let (logValue,err,m') =
              try 
                let (TypedModel(_, rwy) as m') = checkModel (levelOf level) g m tc
                (ModelType rwy,false,m')
              with Failure s ->
                let bogus = TypedModel(MExp(TypedExp(Tabular.Var "bogus",tc)),((T_Record ERT,T_Record ERT),tc))
                (Err s,true,bogus)
            let (TypedModel(_, t)) = m'
            let ((T_Record ws,T_Record zs) as wc,tci) = t 
             //use sup of inferred, not declared, type with det tc
            let (Some tc) = Syntax.supT tci (det tc)
            let g1 = List.fold (fun g (wi,twi) -> envInsertVar g (qualify m c wi) (twi,levelOf level)) g (ws@zs)
            let g2 = envInsertVar g1 c (tc, W)
            let (log',err',tb1') = synthTable isTable g2 tb1
            let log = log'.Add(c,logValue)
            let (tb2, t1) = tb1'
            let col' = (c,{Type = tc; Markup = Param m'})
            let tb' = col'::tb2 
            let (h, w, x, y, z) = t1
            match visibility with
            | Local  ->
              (log,err||err',(tb',(h, w, x, y, z)))
            | Output _ ->
              let w1 = (List.map (fun (wi,twi) ->  (qualify m c wi,twi)) ws) @ (c, tc) :: w
              (log,err||err',(tb',(h, w1, x, y, z)))
            | In ->
              failwith "impossible"
        | Tabular.Input ->
            assert(if isTable then det tc = D else true)
            let tb1:Table = tl
            let g1 = envInsertVar g c (tc, levelOf level)
            let (log',err,tb1') = synthTable isTable g1 tb1
            let (tb2, t1) = tb1'
            let (h, w, x, y, z) = t1
            let x1 = (c, tc) :: x
            let firstCol  = {Type = tc; Markup = (level,visibility,TypedModel(MEmpty,((T_Record ERT,T_Record ERT),tc )))}
            (log',err,(((c, firstCol) :: tb2), (h, w, x1, y, z)))
        | Tabular.Latent m 
        | Tabular.Observable m ->
            //let tc = supT tc R
            let tb1:Table = tl
            let (logValue,err,m') =
              try 
                let (TypedModel(_, rwy) as m') = checkModel (levelOf level) g m tc
                (ModelType rwy,false,m')
              with Failure s ->
                let bogus = TypedModel(MExp(TypedExp(Tabular.Var "bogus",tc)),((T_Record ERT,T_Record ERT),tc))
                (Err s,true,bogus)
            let (TypedModel(_, t)) = m'
            let ((T_Record ws as wc,T_Record zs as zc),tci) = t 
            let (Some tc) = Syntax.supT tci (det tc) //use sup of inferred, not declared, type with det tc
            let g0 = List.fold (fun g (wi,twi) -> envInsertVar g (qualify m c wi) (twi,W)) g ws
            let g1 = List.fold (fun g (wi,twi) -> envInsertVar g (qualify m c wi) (twi,levelOf level)) g0 zs
            let g2 = envInsertVar g1 c (tc, levelOf level)
            let (log',err',tb1') = synthTable isTable g2 tb1
            let log = log'.Add(c,logValue)
            let (tb2, t1) = tb1'
            let (h, w, x, y, z) = t1
            let col' = (c,{Type = tc; Markup = (level,visibility,m')})
            let tb' = col'::tb2
            let err'' = err||err'
            match visibility with
            | In -> failwith "impossible"
            | Local ->
                    (log,err'',(tb',(h,w,x,y,z)))
            | Output _ ->
              let w1 = (List.map (fun (wi,twi) ->  (qualify m c wi,twi)) ws) @ (c, tc) :: w
              let qzs = (List.map (fun (wi,twi) ->  (qualify m c wi,twi)) zs)
              match markup with
              | Latent _ ->
                let z1 = qzs @ (c, tc) :: z
                (log,err'',(tb', (h, w1, x, y, z1)))
              | Observable _ ->
                let z1 = qzs @ z
                let y1 = (c, tc) :: y
                (log,err'',(tb', (h, w1, x, y1, z1)))
        | _ ->
           let bogus = TypedModel(MExp(TypedExp(Tabular.Var "bogus",tc)),((T_Record ERT,T_Record ERT),tc))
           let g1 = envInsertVar g c (tc, levelOf level)
           let (log',err',(tb1',Q)) = synthTable isTable g1 tl
           (log'.Add(c,Err "cannot type model"),
            true,
            (((c,{Type = tc; Markup = (level,visibility,bogus) })::tb1'),Q))
                    
