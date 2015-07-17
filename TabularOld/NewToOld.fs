namespace MicrosoftResearch.Infer.Tabular

module NewToOld =
 module S = MicrosoftResearch.Infer.Tabular.Syntax
 module T = MicrosoftResearch.Infer.Tabular.Tabular

 let rec trExp e = 
   match e with
   | S.Var v -> T.Column v 
   | S.Const (S.IntConst v) -> T.Const v
   | S.Const (S.BoolConst v) -> T.BoolConst v
   | S.Const (S.RealConst v) -> T.RealConst v
   | S.Prim (S.Negate,_) -> failwithf "%A not supported" e
   | S.Prim (S.Not, _) -> failwithf "%A not supported" e
   | S.Prim(S.Plus,[e1;e2]) -> T.Plus(trExp e1,trExp e2)
   | S.Prim(S.Minus,[e1;e2]) -> T.Minus(trExp e1,trExp e2)
   | S.Prim(S.Mult,[e1;e2]) -> T.Times(trExp e1,trExp e2)
   | S.Prim(S.Div,[e1;e2]) -> failwithf "%A not supported" e
   | S.Prim(S.Max,[e1;e2]) ->  failwithf "%A not supported" e
   | S.Prim(S.Mod,[e1;e2]) ->  failwithf "%A not supported" e
   | S.Prim(S.Or,[e1;e2]) -> T.Or(trExp e1,trExp e2)
   | S.Prim(S.And,[e1;e2]) -> failwithf "%A not supported" e
   | S.Prim(S.Eq,[e1;e2]) -> T.EQ(trExp e1,trExp e2)
   | S.Prim(S.Neq,[e1;e2]) -> failwithf "%A not supported" e
   | S.Prim(S.Lt,[e1;e2]) -> failwithf "%A not supported" e
   | S.Prim(S.Gt,[e1;e2]) -> T.GT(trExp e1,trExp e2)
   | S.Prim(S.LtEq,[e1;e2]) -> failwithf "%A not supported" e
   | S.Prim(S.Factor(S.FactorName("Logistic")),[e1]) -> T.Logistic(trExp e1)
   | S.Prim(S.Factor(S.FactorName("Probit")),[e1;e2]) -> T.Probit(trExp e1,trExp e2)
   | S.Prim(S.Factor(S.FactorName("DampBackward")),[e1;e2]) -> T.DampBackward(trExp e1,trExp e2)
   | S.Dist(S.Beta,[e1;e2]) -> T.Beta(trExp e1,trExp e2)
   | S.Dist(S.Bernoulli,[e1]) -> T.Bernoulli(trExp e1)
   | S.Dist(S.GaussianFromMeanAndPrecision,[e1;e2]) -> T.Gaussian(trExp e1,trExp e2)
   | S.Dist(S.GammaFromShapeAndScale,[e1;e2]) -> T.Gamma(trExp e1,trExp e2)
   | S.Dist(S.Dirichlet,[e1;e2]) -> T.Dirichlet(trExp e1,trExp e1)
   | S.Dist(S.Discrete,[e1;e2]) -> T.Discrete(trExp e1,trExp e2)
   | S.Dist(S.DiscreteUniform,[e1]) -> T.DiscreteUniform(trExp e1)
   | S.SizeOf(t) -> T.SizeOf(t)
   | S.DeRef(S.Var e,tn,cn) -> T.Deref(e,T.Column cn) // only works for variable expression
   | S.If(e1,e2,e3) -> T.StrictIf(trExp e1,trExp e2,trExp e3) 
   | S.ForLoop(x,e1,e2) -> T.ForLoop(x,trExp e1,trExp e2) // T.ForLoop exists, but wasn't implemented.
   | S.Array(es) -> T.Arr([| for e in es -> trExp e |])
   | S.Subscript(e1,e2) -> T.Index(trExp e1,trExp e2)
   | S.Let(x,e1,e2) -> T.Let(x,trExp e1,trExp e2) // T.Let exists, but wasn't implemented.
   | _ -> failwithf "%A not supported" e

 let rec trModel m = 
     match m with 
     | S.MExp e -> T.Gen(trExp e)
     | S.MIndexed(m,e1,e2) -> T.Array(trModel m,[trExp e1]) // e2 will be inferred...
     | S.MCall("CGaussian",[]) -> T.CGaussian
     | S.MCall("CBernoulli",[]) -> T.CBernoulli
     | S.MCall("CDiscrete",[("N",e1)]) -> T.CDiscreteWith(trExp e1)
     | S.MCall("CDiscrete",[]) -> T.CDiscreteWith(T.Const(2))
     | _ -> failwithf "%A not supported" m
 let rec trColumnType t = 
     match t with
     | S.T_Link tn -> T.T_Link tn
     | S.T_Real -> T.T_Real 
     | S.T_Bool -> T.T_Bool
     | S.T_Int -> T.T_Int
     | S.T_Upto e1 -> T.T_Upto (trExp e1)
     | S.T_String -> T.T_String
     | S.T_Array (ct,e1) -> T.T_Array(trColumnType ct,trExp e1)
     | S.T_Record flds -> T.T_Record( List.map (fun (v,ty) -> (v,trColumnType ty)) flds)
     | S.T_Vector -> T.T_Vector
 let rec trMarkup m =
     match m with 
       S.Input -> T.Input
     | S.Latent m -> T.Latent (trModel m)
     | S.Observable m -> T.Observable (trModel m)
     | S.Hyper e -> failwithf "%A not supported" m 
     | S.Param m -> T.Latent(T.Static(trModel m))

 let rec trColumns cs = List.map (fun (cn,col:S.Column) -> (cn,{T.Type=trColumnType col.Type;T.Markup=trMarkup col.Markup})) cs
 let rec trTable cs = {T.Columns = trColumns cs}
 let rec trTables decs = 
         match decs with 
         | [] -> []
         | (S.Declaration(S.Table(tn,_),tbl))::decs ->
           (tn,trTable tbl)::trTables decs
         | dec::decs' ->  failwithf "%A not supported" decs
 let rec trSchema name decs = {T.Name = name;T.Tables= trTables decs} : T.Database<T.Markup>