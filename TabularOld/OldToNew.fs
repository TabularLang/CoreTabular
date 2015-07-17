namespace MicrosoftResearch.Infer.Tabular

module OldToNew =
 module S = MicrosoftResearch.Infer.Tabular.Syntax
 module T = MicrosoftResearch.Infer.Tabular.Tabular

 let rec trExp e = 
   match e with
   | T.Column v -> S.Var v  
   | T.Const v -> S.Const (S.IntConst v) 
   | T.DiscreteConst(v1,v2) -> S.Const (S.IntConst v1) 
   | T.BoolConst v -> S.Const (S.BoolConst v)
   | T.RealConst v -> S.Const (S.RealConst v) 
   | T.Plus( e1, e2) -> S.Prim(S.Plus,[trExp e1; trExp e2]) 
   | T.Minus( e1, e2) -> S.Prim(S.Minus,[ trExp e1; trExp e2]) 
   | T.Times( e1, e2) -> S.Prim(S.Mult,[ trExp e1; trExp e2]) 
   | T.Or( e1, e2) -> S.Prim(S.Or,[ trExp e1; trExp e2]) 
   | T.EQ( e1, e2) -> S.Prim(S.Eq,[trExp e1; trExp e2]) 
   | T.GT( e1,  e2) -> S.Prim(S.Gt,[ trExp e1; trExp e2])
   | T.Probit(e1, e2) -> S.Prim(S.Factor(S.FactorName("Probit")),[trExp e1; trExp e2])  
   | T.DampBackward( e1, e2) -> S.Prim(S.Factor(S.FactorName("DampBackward")),[trExp e1;trExp e2]) 
   | T.Beta( e1, e2) -> S.Dist(S.Beta,[ trExp e1;trExp e2]) 
   | T.Bernoulli( e1) -> S.Dist(S.Bernoulli,[ trExp e1]) 
   | T.Gaussian( e1, e2) -> S.Dist(S.GaussianFromMeanAndPrecision,[trExp e1; trExp e2])  
   | T.Gamma( e1, e2) -> S.Dist(S.GammaFromShapeAndScale,[trExp e1;trExp e2]) 
   | T.Dirichlet( e1, e2) -> S.Dist(S.Dirichlet,[ trExp e1;trExp e2]) 
   | T.Discrete( e1, e2) -> S.Dist(S.Discrete,[trExp e1; trExp e2]) 
   | T.DiscreteUniform( e1) -> S.Dist(S.DiscreteUniform,[trExp e1]) 
   | T.SizeOf(t) -> S.SizeOf(t) 
   | T.Deref(e,T.Column cn)  -> S.DeRef(S.Var e,"",cn)  // only works for variable expression
   | T.StrictIf( e1, e2, e3)
   | T.If( e1, e2, e3) -> S.If(trExp e1,trExp e2,trExp e3) 
   | T.ForLoop(x, e1, e2)  -> S.ForLoop(x, trExp e1, trExp e2) // T.ForLoop exists, but wasn't implemented.
   | T.Arr(es) -> S.Array([ for e in es -> trExp e ] )
   | T.Index(e1,e2) -> S.Subscript( trExp e1, trExp e2) 
   |  T.Let(x, e1, e2) -> S.Let(x,trExp e1, trExp e2) // T.Let exists, but wasn't implemented.
   | _ -> failwithf "%A not supported" e
    

 let rec trModel m = 
     match m with 
     | T.Gen( e) -> S.MExp (trExp e)
     | T.Hyper( e) -> S.MExp (trExp e)
     | T.Array( m,[e1]) -> S.MIndexed(trModel m, trExp e1,S.Const (S.IntConst -1)) // -1 is a dummy
     | T.Array( m,((_::_) as es)) ->
           let (e1::es) = List.rev es 
           S.MIndexed(trModel (T.Array( m,List.rev es)), trExp e1,S.Const (S.IntConst -1))
     | T.CGaussian -> S.MCall("CGaussian",[]) 
     | T.CBernoulli -> S.MCall("CBernoulli",[]) 
     | T.CDiscreteWith( e1) -> S.MCall("CDiscrete",[("N",trExp e1)]) 
     | T.CDiscreteWith(T.Const(2)) -> S.MCall("CDiscrete",[])  
     | _ -> failwithf "%A not supported" m
     
 let rec trColumnType t = 
     match t with
     | T.T_Link tn -> S.T_Link tn  
     | T.T_Real  -> S.T_Real 
     | T.T_Bool -> S.T_Bool
     | T.T_Int -> S.T_Int 
     | T.T_Upto ( e1) ->  S.T_Upto (trExp e1)
     | T.T_String -> S.T_String 
     | T.T_Array( ct, e1) -> S.T_Array (trColumnType ct, trExp e1) 
     | T.T_Record flds ->  S.T_Record ( List.map (fun (v,ty) -> (v,trColumnType ty)) flds)
     | T.T_Vector -> S.T_Vector 
     | _ -> failwithf "%A not supported" t
 let rec trMarkup m =
     match m with 
     |  T.Input -> S.Input
     | T.Latent (T.Hyper e) | T.Observable (T.Hyper e)   -> S.Hyper (trExp e)
     | T.Latent (T.Static m) | T.Observable (T.Static m)   -> S.Param (trModel m )
     | T.Latent m -> S.Latent (trModel m)
     | T.Observable m -> S.Observable (trModel m)
     | _ -> failwithf "%A not supported" m

 let rec trColumns cs = List.map (fun (cn,col:T.Column<_>) -> (cn,{S.Type=trColumnType col.Type;S.Markup=trMarkup col.Markup})) cs
 let rec trTable cs = trColumns cs
 let rec trTables decs = 
         match decs with 
         | [] -> []
         | ((tn,{T.Columns =tbl}))::decs ->
           S.Declaration(S.Table(tn,None),trTable tbl)::trTables decs
        // | dec::decs' ->  failwith "%A not supported" decs
 let rec trSchema {T.Name = name;T.Tables= tbs} = trTables tbs