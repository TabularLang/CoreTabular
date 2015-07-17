
namespace MicrosoftResearch.Infer.Tabular

#if DEAD

open MicrosoftResearch.Infer
open MicrosoftResearch.Infer.Models
open MicrosoftResearch.Infer.Distributions
open MicrosoftResearch.Infer.Collections
open System.Linq.Expressions

open Syntax
module FArray = Microsoft.FSharp.Collections.Array

type Helper =  
    static member Sum(a:double[]) = Array.sum a
    static member StringEq(a:string,b:string) = a = b
    static member max(a:double,b:double) = if a >= b then a else b
    static member ArgMax(a:double[]) = 
                 let rec maxi cm n =
                     if n = a.Length then cm
                     else let cm' = if a.[cm] >= a.[n] then cm else n
                          in maxi cm' (n+1)
                 in
                    maxi 0 0

    static member HashMax(a:double[]) =
                  Array.max(a)
   
    static member ArgMin(a:double[]) =
                 let rec mini cm n =
                     if n = a.Length then cm
                     else let cm' = if a.[cm] <= a.[n] then cm else n
                          in mini cm' (n+1)
                 in
                    mini 0 0 
    static member DiscreteProbs(d:obj):real [] =
           let counts = (d:?>Distributions.Discrete).GetProbs()
           Array.init counts.Count (fun i -> counts.[i])    

    static member DirichletCounts(d:obj):real [] =
           let counts = (d:?>Distributions.Dirichlet).PseudoCount
           Array.init counts.Count (fun i -> counts.[i])    

    static member Logistic(v:double) =
           exp(v) / (1.0+exp(v)) 

    static member Softmax(v:double[]) : Maths.Vector =
           Maths.MMath.Softmax(v)
         
    static member DiagonalPDMatrix(v:real[]) : Maths.PositiveDefiniteMatrix = 
                  let mat = Maths.PositiveDefiniteMatrix.Identity(v.Length)
                  let _ = mat.SetDiagonal(Maths.Vector.FromArray(v)) 
                  mat

    static member IndexDist(dists:obj,idx:int) =
                  let mr = dists.GetType().GetMethod("get_Item")
                  mr.Invoke(dists,[|box idx|])
                  //match dists with
                  //dists.GetType()
                  //| :? System.Collections.IList as l -> l.[idx] 
                  //| _ -> failwithf "IndexDist"

    static member Bernoulli(bias:real) =
                  Distributions.Bernoulli(bias).Sample()
    static member Beta(alpha,beta) =
                  Distributions.Beta(alpha,beta).Sample()
    static member BetaFromMeanAndVariance(mean,variance) =
                  Distributions.Beta.FromMeanAndVariance(mean,variance).Sample()
    static member GaussianFromMeanAndPrecision(mean,prec) =
                  Distributions.Gaussian.FromMeanAndPrecision(mean,prec).Sample()
    static member GaussianFromMeanAndVariance(mean,var) =
                  Distributions.Gaussian.FromMeanAndVariance(mean,var).Sample()
   
    static member GammaFromShapeAndScale(shape,scale) =
                  Distributions.Gamma.FromShapeAndScale(shape,scale).Sample()
    static member GammaFromShapeAndRate(shape,rate) =
                  Distributions.Gamma.FromShapeAndRate(shape,rate).Sample()
    static member GammaFromMeanAndVariance(mean,var) =
                  Distributions.Gamma.FromMeanAndVariance(mean,var).Sample()             
    static member Binomial(trials,probSuccess) =
                  Distributions.Binomial(trials,probSuccess).Sample() 
    static member VectorGaussianFromMeanAndPrecision(mean:Maths.Vector,prec) =
                  Distributions.VectorGaussian(mean,prec).Sample()
    static member VectorGaussianFromMeanAndVariance(mean:Maths.Vector,var) =
                  Distributions.VectorGaussian.FromMeanAndVariance(mean,var).Sample()
    static member Discrete(count:int,probs:Maths.Vector) =
                  Distributions.Discrete(probs.ToArray()).Sample()
    static member DiscreteUniform(count:int) =
                  Distributions.Discrete.Uniform(count).Sample()
    static member Poisson(mean) =
                  Distributions.Poisson(mean:float).Sample()
    static member Dirichlet(counts:float[]) =
                  Distributions.Dirichlet(counts).Sample()
    static member DirichletUniform(count) =
                  Distributions.Dirichlet.Uniform(count).Sample() 
    static member DirichletSymmetric(count,alpha) =
                  Distributions.Dirichlet.Symmetric(count,alpha).Sample() 
    static member WishartFromShapeAndRate(shape,rate) =
                  Distributions.Wishart.FromShapeAndRate(shape,rate).Sample()
    static member WishartFromShapeAndScale(shape,rate) =
                  Distributions.Wishart.FromShapeAndScale(shape,rate).Sample()
    static member StdDeviation(dist:obj):double =
                  let v = (dist :?> CanGetVariance<double>).GetVariance()
                  System.Math.Sqrt(v)
 
(*
  | Beta | BetaFromMeanAndVariance 
    | GaussianFromMeanAndPrecision | GaussianFromMeanAndVariance
    | GammaFromShapeAndScale | GammaFromMeanAndVariance | GammaFromShapeAndRate
    | Binomial
    | VectorGaussianFromMeanAndVariance | VectorGaussianFromMeanAndPrecision 
    | Discrete | DiscreteUniform
    | Poisson
    | Bernoulli
    | Dirichlet | DirichletUniform | DirichletSymmetric
    | WishartFromShapeAndRate | WishartFromShapeAndScale
*)


module QueryCompilerOld = 

  
  type X = System.Linq.Expressions.Expression
  type XT = System.Linq.Expressions.ExpressionType

 
  
  // these abbreviations are a hack to enable easy cut-n-past from the compiler code
  [<AbstractClass>]
  type XExp(e:Expression) = 
       abstract member MakeArray: unit -> XExp 
       abstract member e: Expression 
  type XExp<'T>(e:Expression) =
       inherit XExp(e)
       override this.MakeArray() = new XExp<'T[]>(e) :> XExp
       override this.e = e
  type XVar = XExp

  let rec xexpOf T e  = 
       match T with
       | T_Upto _ 
       | T_Int -> new XExp<int>(e) :> XExp
       | T_Bool -> new XExp<bool>(e) :> XExp
       | T_Real ->  new XExp<double>(e) :> XExp
       | T_String -> new XExp<string>(e) :> XExp
       | T_PositiveDefiniteMatrix -> new XExp<Maths.PositiveDefiniteMatrix>(e) :> XExp
       | T_Vector -> new XExp<Maths.Vector>(e) :> XExp
       | T_Array(T,_) -> (xexpOf T e).MakeArray() // refactor
       | _ -> failwithf "xexpOfT %A" T

  let rec typeOf T : System.Type = 
       match T with
       | T_Upto _  
       | T_Int -> typeof<int>
       | T_Bool -> typeof<bool>
       | T_Real ->  typeof<double>
       | T_String -> typeof<string>
       | T_PositiveDefiniteMatrix -> typeof<Maths.PositiveDefiniteMatrix>
       | T_Vector -> typeof<Maths.Vector>
       | T_Array(T,_) -> (typeOf T).MakeArrayType()
       | _ -> failwithf "typeOf %A" T

  type System.Linq.Expressions.Expression with
       member this.distProperty<'Dist,'T>(name) =  
           let mr = match (typeof<'Dist>).GetProperty(name,typeof<'T>) with
                    | null -> (typeof<'Dist>).GetField(name) :> System.Reflection.MemberInfo
                    | mr -> mr :> System.Reflection.MemberInfo
           new XExp<'T>(X.MakeMemberAccess(X.ConvertChecked(this,typeof<'Dist>),mr))
          
       member this.distMethod<'Dist,'T>(name) =  
           let mr = (typeof<'Dist>).GetMethod(name,[||])
           new XExp<'T>(X.Call(X.ConvertChecked(this,typeof<'Dist>),mr))

      (*
       static member LetArray(t,x,c:int,e) =
          let exit = X.Label(typeof<System.Void>)
          let a = X.Variable(typeOf t)
          let xv = X.Variable(typeof<int>,x)
          variableOf t (
           X.Block(
            [| a;xv |],
            [  X.Assign(a,X.Constant(0)) :> X;
               X.Assign(a,X.NewArrayBounds(typeOf t,[|X.Constant(box c):>X|])) :> X;
               X.Loop(X.IfThenElse(
                        X.GreaterThanOrEqual(xv,X.ArrayLength(a) :> X),
                        X.Break(exit),
                        X.Block(X.Assign(X.ArrayAccess(a,xv),
                                         e),
                                X.PreIncrementAssign(xv))
                        ),exit) :> X;
           //    X.Label(exit) :> X;
               a :> X
            ] :> seq<Expression>
          ))
      *)

  let interp_Gt (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
        new XExp<bool>(X.MakeBinary(XT.GreaterThan,v.e,w.e)) :> XExp
      | (:? XExp<double> as v),(:? XExp<double> as w) ->
        new XExp<bool>(X.MakeBinary(XT.GreaterThan,v.e,w.e)) :> XExp
  let inline interp_GtEq (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
        new XExp<bool>(X.MakeBinary(XT.GreaterThanOrEqual,v.e,w.e)) :> XExp
      | (:? XExp<double> as v),(:? XExp<double> as w) ->
        new XExp<bool>(X.MakeBinary(XT.GreaterThanOrEqual,v.e,w.e)) :> XExp

  let inline interp_Lt (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
        new XExp<bool>(X.MakeBinary(XT.LessThan,v.e,w.e)) :> XExp
      | (:? XExp<double> as v),(:? XExp<double> as w) ->
        new XExp<bool>(X.MakeBinary(XT.LessThan,v.e,w.e)) :> XExp
  let inline interp_LtEq (v:XExp) (w:XExp) =  
      match v,w with
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
        new XExp<bool>(X.MakeBinary(XT.LessThanOrEqual,v.e,w.e)) :> XExp
      | (:? XExp<double> as v),(:? XExp<double> as w) ->
        new XExp<bool>(X.MakeBinary(XT.LessThanOrEqual,v.e,w.e)) :> XExp
  let inline interp_Plus (v:XExp) (w:XExp) = 
    match v,w with
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
        new XExp<int>(X.MakeBinary(XT.Add,v.e,w.e)) :> XExp
      | (:? XExp<double> as v),(:? XExp<double> as w) ->
        new XExp<double>(X.MakeBinary(XT.Add,v.e,w.e)) :> XExp
  let inline interp_Minus (v:XExp) (w:XExp) = 
    match v,w with
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
        new XExp<int>(X.MakeBinary(XT.Subtract,v.e,w.e)) :> XExp
      | (:? XExp<double> as v),(:? XExp<double> as w) ->
        new XExp<double>(X.MakeBinary(XT.Subtract,v.e,w.e)) :> XExp
  let inline interp_Eq (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
       new XExp<bool>(X.MakeBinary(XT.Equal,v.e,w.e)) :> XExp
      | (:? XExp<string> as v),(:? XExp<string> as w) ->
       new XExp<bool>( X.Call(typeof<Helper>.GetMethod("StringEq"),[|v.e;w.e|])) :> XExp      
      | (:? XExp<bool> as v),(:? XExp<bool> as w) ->
       new XExp<bool>(X.MakeBinary(XT.Equal,v.e,w.e)) :> XExp
  let inline interp_Neq (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
       new XExp<bool>(X.MakeBinary(XT.NotEqual,v.e,w.e)) :> XExp
      | (:? XExp<bool> as v),(:? XExp<bool> as w) ->
       new XExp<bool>(X.MakeBinary(XT.NotEqual,v.e,w.e)) :> XExp
  let inline interp_And(v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<bool> as v),(:? XExp<bool> as w) ->
          new XExp<bool>(X.MakeBinary(XT.AndAlso,v.e,w.e)):> XExp
  let inline interp_Or (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<bool> as v),(:? XExp<bool> as w) ->
        new XExp<bool>(X.MakeBinary(XT.OrElse,v.e,w.e)):> XExp
  let inline interp_Not (v:XExp) = 
      match v with
      | (:? XExp<bool> as v)->
        new XExp<bool>(X.MakeUnary(XT.Not,v.e,typeof<bool>)):> XExp
  let inline interp_Negate (v:XExp) = 
      match v with
      | (:? XExp<double> as v)->
         new XExp<double>(X.MakeUnary(XT.Negate,v.e,typeof<bool>)):> XExp 
  let inline interp_Mult (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<double> as v),(:? XExp<double> as w) ->
         new XExp<double>(X.MakeBinary(XT.Multiply,v.e,w.e)):> XExp      
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
         new XExp<int>(X.MakeBinary(XT.Multiply,v.e,w.e)):> XExp 

  let inline interp_Max (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<double> as v),(:? XExp<double> as w) ->
      new XExp<double>( X.Call(typeof<Helper>.GetMethod("max"),[|v.e;w.e|])) :> XExp
          
  let inline interp_Div (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<double> as v),(:? XExp<double> as w) ->
             new XExp<double>(X.MakeBinary(XT.Divide,v.e,w.e)):> XExp      
  let inline interp_Mod (v:XExp) (w:XExp) = 
      match v,w with
      | (:? XExp<int> as v),(:? XExp<int> as w) ->
       new XExp<int>(X.MakeBinary(XT.Modulo,v.e,w.e)):> XExp   

  let interp_Logistic (v:XExp) = 
      match v with
      | (:? XExp<real> as v) ->
          new XExp<double>( X.Call(typeof<Helper>.GetMethod("Logistic"),[|v.e|])) :> XExp
         
             

  
  let rec interpPrim p es : XExp = 
        match (p,es) with
        | Prim.Gt,[e1;e2] -> interp_Gt e1 e2
        | Prim.Lt,[e1;e2] -> interp_Lt e1 e2
        | Prim.GtEq,[e1;e2] -> interp_GtEq e1 e2
        | Prim.LtEq,[e1;e2] -> interp_LtEq e1 e2
        | Prim.Eq,[e1;e2]  -> 
              interp_Eq e1 e2
        | Prim.Neq,[e1;e2]  -> 
              interp_Neq e1 e2
        | Prim.Mult,[e1;e2]  -> 
              interp_Mult e1 e2
        | Prim.Div,[e1;e2]  -> 
              interp_Div e1 e2
        | Prim.Mod,[e1;e2]  -> 
              interp_Mod e1 e2
        | Prim.Max,[e1;e2]  -> 
              interp_Max e1 e2
        | Prim.Minus,[e1;e2]  -> 
              interp_Minus e1 e2
        | Prim.Negate,[e1]  -> 
              interp_Negate (e1)
        | Prim.Not,[e1]  -> 
              interp_Not (e1)
        | Prim.Plus,[e1;e2]  -> 
              interp_Plus e1 e2
        | Prim.Factor(FactorName"DampBackward"),[e1;e2] -> 
           e1
        | Prim.Factor(FactorName"Logistic"),[e1] -> 
           interp_Logistic e1
        | Prim.Factor(FactorName"Probit"),[e1;e2] ->  
           failwithf "random Probit use at query level"
        | Prim.Factor(FactorName"Sum"),[e1] ->  
           match (e1) with
           |  (:? XExp<double[]> as v1) ->
               new XExp<double>( X.Call(typeof<Helper>.GetMethod("Sum"),[|v1.e|]))
                :> XExp
        | Prim.Factor(FactorName "Softmax"),[e1] ->  
           match (e1) with
           |  (:? XExp<double[]> as v1) ->
               new XExp<Maths.Vector>( X.Call(typeof<Helper>.GetMethod("SoftMax"),[|v1.e|])) :> XExp
        | Prim.Factor(FactorName"InnerProduct"),[e1;e2] ->  
           match (e1,e2) with
           |  (:? XExp<Maths.Vector> as v1),
              (:? XExp<Maths.Vector> as v2) ->
              new XExp<double>(
                 X.Call(typeof<Maths.Vector>.GetMethod("InnerProduct"),[|v1.e;v2.e|]))
              :> XExp
        | Prim.Factor(FactorName"VectorFromArray"),[e1] ->  
           match e1 with
           |  (:? XExp<double[]> as v1) ->
                 new XExp<Maths.Vector>(
                  X.Call(typeof<Maths.Vector>.GetMethod("FromArray",[|typeof<double[]>|]),[|v1.e|]))
                :> XExp
        | Prim.Factor(FactorName"Exp"),[e1] ->  
           match e1 with
           |  (:? XExp<double> as v1) ->
                new XExp<double>(
                  X.Call(typeof<System.Math>.GetMethod("Exp",[|typeof<double>|]),[|v1.e|]))
                :> XExp
        | Prim.Factor(FactorName"Log"),[e1] ->  
           match e1 with
           |  (:? XExp<double> as v1) ->
                new XExp<double>(
                  X.Call(typeof<System.Math>.GetMethod("Log",[|typeof<double>|]),[|v1.e|]))
                :> XExp
        | Prim.Factor(FactorName "DiagonalPDMatrix"),[e1] ->  
           match e1 with
           |  (:? XExp<real[]> as v1)   ->
            new XExp<Maths.PositiveDefiniteMatrix>(
                 X.Call(typeof<Helper>.GetMethod("DiagonalPDMatrix",[|typeof<real[]>|]),[|v1.e|])):> XExp
                
        | Prim.Factor(FactorName "IdentityScaledBy"),[e1;e2]->  
            match (e1,e2) with
            |  (:? XExp<int> as v1),
               (:? XExp<double> as v2) ->
               new XExp<Maths.PositiveDefiniteMatrix>(
                  X.Call(typeof<Maths.PositiveDefiniteMatrix>.GetMethod("IdentityScaledBy",[|typeof<int>;typeof<double>|]),[|v1.e;v2.e|]))
                :> XExp
            
       (*
        | Prim.Factor(FactorName "GetItems"),[e1;e2]->  // I doubt this will work...
            let v = e1
            let w = e2
            let m = typeof<Variable>.GetGenericMethod("GetItems", [|  v.GetType().GetGenericArguments().[0]|])
            let _ =  m.Invoke(v, [| i; w |])
            ()
        | Prim.Factor(FactorName "SubArray"),[e1;e2]->  // I doubt this will work...
            let v = e1
            let w = e2
            let m = typeof<Variable>.GetGenericMethod("GetItems", [|  v.GetType().GetGenericArguments().[0]|])
            let _ =  m.Invoke(v, [| i; w |])
            ()
        *)
 
        | Prim.Factor(FactorName "ArgMax"),[e1] ->
          match (e1) with
           |  (:? XExp<double[]> as v1) ->
                new XExp<int>( X.Call(typeof<Helper>.GetMethod("ArgMax"),[|v1.e|]))
                :> XExp
        | Prim.Factor(FactorName "ArgMin"),[e1] ->
          match (e1) with
           |  (:? XExp<double[]> as v1) ->
               new XExp<int>( X.Call(typeof<Helper>.GetMethod("ArgMin"),[|v1.e|]))
               :> XExp
        | Prim.Factor(FactorName "BreakSymmetry"),[e1] -> //erase
            e1
        | Prim.Factor(FactorName "#Max"),[e1] ->
           match (e1) with
           |  (:? XExp<double[]> as v1) ->
               new XExp<double>( X.Call(typeof<Helper>.GetMethod("HashMax"),[|v1.e|]))
               :> XExp
        | Prim.Factor(FactorName "#Sqrt"),[e1] ->
           match e1 with
           |  (:? XExp<double> as v1) ->
                new XExp<double>(
                  X.Call(typeof<System.Math>.GetMethod("Sqrt",[|typeof<double>|]),[|v1.e|]))
                :> XExp
        | Prim.Factor(FactorName "#Round"),[e1] ->
          match e1 with
           |  (:? XExp<double> as v1) ->
                new XExp<double>(
                  X.Call(typeof<System.Math>.GetMethod("Round",[|typeof<double>|]),[|v1.e|]))
                :> XExp                  
        | Prim.Factor(FactorName f),es -> failwithf "interpPrim %A not yet implemented" f

  let tt = new XExp<bool>(X.Constant(true)) :> XExp
  let ff = new XExp<bool>(X.Constant(false)) :> XExp

  // the index into the lambda's parameter list, used to extract out arguments
  type index = int
  type mode = MSampler | MQuery

  let rec
   trE mode (TE:Map<TableName,(int*Map<ColumnName, Syntax.B * Syntax.D * XVar * index>)>) (CE:Map<ColumnName, Syntax.B * Syntax.D * XVar * index>) (i:XVar) et  : XExp = 
      let trE = trE mode
      let (TypedExp(e,t)) = et 
      match e with 
      | Var x -> match CE.[x] with
                 | (H,_,v,_) -> v
                 | (W,_,v,_) -> v
                 | (Y,D.R,vs,_) when mode = MQuery -> 
                  new XExp<obj>( X.ArrayIndex(vs.e,i.e) (* X.Call(typeof<Helper>.GetMethod("IndexDist"),[|vs.e;i.e|])*)) :> XExp
                 | (Y,_,vs,_) -> 
                    xexpOf t (X.ArrayIndex(vs.e,i.e)) 
      | Const (IntConst v) -> new XExp<int>( X.Constant(v)) :> XExp
      | Const (BoolConst v) ->new XExp<bool>( X.Constant(v)) :> XExp
      | Const (RealConst v) -> new XExp<double>( X.Constant(v)) :> XExp
      | Const (StringConst v) -> new XExp<string>( X.Constant(v)) :> XExp
      | SizeOf(tn) -> new XExp<int>( X.Constant(fst (TE.[tn]))) :> XExp
      | Array es -> 
           xexpOf t (X.NewArrayInit((typeOf t).GetElementType(), [ for e in es -> (trE TE CE i e).e ]))
            
      | Prim(Prim.And,[e1;e2])  -> 
             interp_And (trE TE CE i e1) (trE TE CE i e2)
      | Prim(Prim.Or,[e1;e2])  -> 
             interp_Or (trE TE CE i e1) (trE TE CE i e2)
      | Prim(p,es) -> interpPrim p (List.map (trE TE CE i) es)
      | Dist(d,es) -> 
         let sampler = typeof<Helper>.GetMethod(sprintf "%A" d)
         assert not (sampler = null)
         xexpOf t ( X.Call(sampler,[| for e in es -> (trE TE CE i e).e |]))
      | Let(x,(TypedExp(_,t1) as e1),e2) ->
         let v1 = trE TE CE i e1 in
         let xv = X.Variable(typeOf t1, x)
         xexpOf t (
          X.Block([xv],
                  [X.Assign(xv,v1.e) :> X;
                   (trE TE (CE.Add(x,(W,D,xexpOf t1 xv,-1))) i e2).e
                  ]) :> X
         )
      | DeRef(e,tn,cn) -> 
         let v = trE TE CE i e in
         match v, (snd(TE.[tn])).[cn] with
         | :? XExp<int> as k, (_,_,a,_) ->
             xexpOf t (X.ArrayIndex(a.e,k.e))
      | Ref(tn,cn) -> 
         match (snd(TE.[tn])).[cn] with
         | (_,_,a,_) ->
           a
      | If (e1,e2,e3) ->
         let v1 = trE TE CE i e1 in
         let v2 = trE TE CE i e2 in
         let v3 = trE TE CE i e3 in
         match v1 with
         | :? XExp<bool> as b ->
           xexpOf t (X.Condition(v1.e,v2.e,v3.e,typeOf t))
      | ForLoop(x,e1,e2) ->
         let (TypedExp(_,t2)) = e2
         let v1 = trE TE CE i e1 
         match v1 with
         | :? XExp<int> as b ->
          let exit = X.Label(typeof<System.Void>)
          let cont = X.Label(typeof<System.Void>)
          let a = X.Variable(typeOf t)
          let xv = X.Variable(typeof<int>,x)
          xexpOf t (
           X.Block(
            [| a;xv |],
            [  X.Assign(xv,X.Constant(0)) :> X;
               X.Assign(a,X.NewArrayBounds(typeOf t2,[|v1.e|])) :> X;
               X.Loop(X.IfThenElse(
                        X.GreaterThanOrEqual(xv,X.ArrayLength(a)),
                        X.Break(exit),
                        X.Block(X.Assign(X.ArrayAccess(a,xv),
                                             (trE TE  (CE.Add(x,(W,D,new XExp<int>(xv):>XVar,-1))) i e2).e),
                                X.PreIncrementAssign(xv),
                                X.Continue(cont))
                        ),exit,cont) :> X;
               a :> X
            ] :> seq<Expression>
          ))
      | Subscript(e1,e2) ->
         let v1 = trE TE CE i e1 in
         let v2 = trE TE CE i e2 in
         match v1 with
         | :? XExp<obj> -> // better be a distribution array
            new XExp<obj>( X.Call(typeof<Helper>.GetMethod("IndexDist"),[|v1.e;v2.e|])) :> XExp
         | _ -> xexpOf t (X.MakeBinary(XT.ArrayIndex,v1.e,v2.e))
      | Constraint(e1,t1) ->
         trE TE CE i e1 
      | Infer((Beta|BetaFromMeanAndVariance),[],("alpha"|"trueCount"),e1) ->
         let v1 = trE TE CE i e1 
         let b = Distributions.Beta()
         v1.e.distProperty<Distributions.Beta,double>("TrueCount") :> XExp
         
      | Infer((Beta|BetaFromMeanAndVariance),[],("beta"|"falseCount"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distProperty<Distributions.Beta,double>("FalseCount") :> XExp
      | Infer((Beta|BetaFromMeanAndVariance),[],("Mean"|"mean"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distMethod<CanGetMean<double>,double>("GetMean") :> XExp
      | Infer((Beta|BetaFromMeanAndVariance),[],("Variance"|"variance"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distMethod<CanGetVariance<double>,double>("GetVariance") :> XExp
      | Infer(Bernoulli,[],("Bias"|"bias"|"Mean"|"mean"|"probTrue"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distMethod<CanGetMean<double>,double>("GetMean") :> XExp
      | Infer((GaussianFromMeanAndVariance|GaussianFromMeanAndPrecision),[],("Mean"|"mean"),e1) ->
         let v1 = trE TE CE i e1
         v1.e.distMethod<CanGetMean<double>,double>("GetMean") :> XExp
      | Infer((GaussianFromMeanAndVariance|GaussianFromMeanAndPrecision),[],("Variance"|"variance"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distMethod<CanGetVariance<double>,double>("GetVariance") :> XExp
      | Infer((GaussianFromMeanAndVariance|GaussianFromMeanAndPrecision),[],("Precision"|"precision"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distProperty<Distributions.Gaussian,double>("Precision") :> XExp 
      | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Mean"|"mean"),e1) ->
         let v1 = trE TE CE i e1
         v1.e.distMethod<CanGetMean<double>,double>("GetMean") :> XExp
      | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Variance"|"variance"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distMethod<CanGetVariance<double>,double>("GetVariance") :> XExp
      | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Rate"|"rate"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distProperty<Distributions.Gamma,double>("Rate") :> XExp 
      | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Shape"|"shape"),e1) ->
         let v1 = trE TE CE i e1
         v1.e.distProperty<Distributions.Gamma,double>("Shape") :> XExp 
      | Infer((GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("Scale"|"scale"),e1) ->
         let v1 = trE TE CE i e1
         v1.e.distMethod<Distributions.Gamma,double>("GetScale") :> XExp 
      | Infer((Dist.Dirichlet|Dist.DirichletSymmetric|DirichletUniform),[e0],("counts"|"Counts"),e1) -> 
         let v1 = trE TE CE i e1 
         new XExp<double[]>( X.Call(typeof<Helper>.GetMethod("DirichletCounts"),[|v1.e|])) :> XExp
      | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Probs"|"probs"),e1) -> 
         let v1 = trE TE CE i e1 
         new XExp<double[]>( X.Call(typeof<Helper>.GetMethod("DiscreteProbs"),[|v1.e|])) :> XExp
      | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Mode"|"mode"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distMethod<CanGetMode<int>,int>("GetMode") :> XExp //NB: not a constructor argument
      | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Median"|"median"),e1) ->
         let v1 = trE TE CE i e1 
         v1.e.distMethod<Distributions.Discrete,int>("GetMedian") :> XExp 
      | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Mean"|"mean"),e1) ->
         let v1 = trE TE CE i e1
         v1.e.distProperty<Distributions.Discrete,double[]>("GetMean") :> XExp  
      | Infer((GaussianFromMeanAndVariance|GaussianFromMeanAndPrecision|GammaFromMeanAndVariance|GammaFromShapeAndRate|GammaFromShapeAndScale),[],("StdDeviation"),e1) ->
         let v1 = trE TE CE i e1
         new XExp<double>(X.Call(typeof<Helper>.GetMethod("StdDeviation"),[|v1.e|])) :> XExp
        

  let trModel mode TE CE i m = 
      match m with
      | TypedModel(MExp e,_) ->
        trE mode TE CE i e
      | _ -> failwith "non-core model"

  
  let rec schemaHasQuery
            hasQuery
            tables =
      match tables with
      | [] -> hasQuery
      | (Declaration(Fun tn,table)::tables) ->  schemaHasQuery hasQuery tables
      | (Declaration(Table(tn,_),table)::tables) ->
        let rec tableHasQuery hasQuery columns  =
          match columns with
          |  [] -> 
            schemaHasQuery hasQuery tables
          | (cn,{Type=T;Markup=m})::rest ->
            tableHasQuery (hasQuery || Types.det T = Qry) rest
        tableHasQuery hasQuery table
  
 

  #if DEAD
  let trData TE =
            let distDTO = DistDTO (TE |> Map.map(fun tn (tblsize,colmap) -> 
                                             let colmap = Map.filter (fun cn (b,d,o) -> b > B.W) colmap 
                                             let cols = Seq.toList(colmap.Keys)
                                             let rowsize = Seq.length colmap.Keys
                                             let c2i = cols |> List.mapi (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                             let dists = Microsoft.FSharp.Collections.Array.create<System.Array> rowsize null
                                             let _ = Map.iter (fun cn (b,d,o:Query.value) -> 
                                                                  let a = o :?> System.Array
                                                                  dists.[c2i.[cn]] <- a (*o :?> System.Array*)) colmap
                                             let tbl = [| for row in 0..tblsize-1 -> Microsoft.FSharp.Collections.Array.init rowsize (fun i -> dists.[i].GetValue(row))  |]
                                             (c2i, tbl :> seq<obj[]>)))
            let knowDTO = KnowDTO (TE |> Map.map (fun tn (size,colmap) ->
                                                     let colmap = Map.filter (fun cn (b,d,o) -> b = B.W) colmap
                                                     let cols = Seq.toList(colmap.Keys)
                                                     let c2i = cols |> List.mapi (fun i c -> (i,c)) |> Seq.fold (fun m (i,c) -> Map.add c i  m) Map.empty
                                                     let array = [| for cn in cols -> let (b,d,o) = colmap.[cn] in o |]
                                                     (c2i,
                                                      array)))
            (distDTO,knowDTO)

  let rec Sample dto
            tables  = 
            let res = trTables MSampler dto (DistDTO  Map.empty) (KnowDTO Map.empty) [] [] [] [] Map.empty tables
            trData res

  and  Query dto distDTO knowDTO
            tables = 
            let res = trTables MQuery dto distDTO knowDTO [] [] [] [] Map.empty tables
            trData res

  and trTables mode (DTO dto) (DistDTO distDTO) (KnowDTO knowDTO)
            pars decs exps args
            (TE: Map<TableName,(int*Map<ColumnName, Syntax.B * Syntax.D * XVar * index>)>)
            tables  = 
      let trTables =  trTables mode (DTO dto) (DistDTO distDTO) (KnowDTO knowDTO) 
      match tables with
      | [] -> let lam = X.Lambda(X.Block(List.rev decs,List.rev exps),List.rev pars)
              //System.Console.WriteLine(lam)
              let del = lam.Compile()
              let args = List.toArray(List.rev args)
              let inargs = Array.copy args
              let res = del.DynamicInvoke(args)
              let outargs = args
              let data = Map.map (fun  tn (size_tn,colmap) -> (size_tn,Map.fold (fun m cn info ->
                                                                             match info with 
                                                                             | (b,d,_,-1) -> m
                                                                             | (b,d,_,index) -> Map.add cn (b,d,outargs.[index]) m) Map.empty colmap)) TE
              data
              
      | (Declaration(Table(tn,_),table)::tables) -> 
      (* todo, refactor me *)
        let size = dto.[tn]
        let (colmap,data) = dto.[tn]
        let data = Seq.toArray(data)
        let length = data.Length
        //assert (length = size)
        let choosei fchoose ar =  ar |> FArray.mapi(fun i e -> (i,e))|> FArray.choose fchoose 
        let mkArray c cty  =
          let i = colmap.[c]
          let observedIndices = data |> choosei (fun (k,r) -> if (r.[i] = null) then None else Some k)
          observedIndices,
          match cty with 
          // note: unsafe conversions 
          | T_Link tn ->  ([| for irow in observedIndices -> System.Convert.ToInt32  (data.[irow].[i])|] :> System.Array)
          | T_Real ->     ([| for irow in observedIndices -> System.Convert.ToDouble (data.[irow].[i])|] :> System.Array)
          | T_Int ->      ([| for irow in observedIndices -> System.Convert.ToInt32  (data.[irow].[i])|] :> System.Array)
          | T_Bool ->     ([| for irow in observedIndices -> System.Convert.ToBoolean(data.[irow].[i])|] :> System.Array)
          | T_Upto _ ->   ([| for irow in observedIndices -> System.Convert.ToInt32  (data.[irow].[i])|] :> System.Array)
          | T_String _ -> ([| for irow in observedIndices -> System.Convert.ToString (data.[irow].[i])|] :> System.Array)
          | T_Array(ety,e) -> failwith "NYI"
          | T_Record _  -> failwith "NYI"
          | T_Vector -> failwith "NYI"
        (* *)
        let rec trColumns pars decs exps args CE   =
         fun columns ->
          match columns with
          |  [] -> 
            trTables pars decs exps args (TE.Add(tn,(length,CE))) tables
          | (cn,{Type=T;Markup=m})::rest ->
            if mode = MSampler && Types.det T = Qry 
            then trColumns pars decs exps args CE rest
            else
            match m with
            | Hyper e ->
               let dec = X.Parameter(typeOf T,cn)
               let exp = trE TE CE (XExp<int>(X.Constant(-1)):>XExp) e
               let CE' = CE.Add(cn,(H,D,xexpOf T (dec:>X),-1))
               let decs = dec::decs
               let exps = (X.Assign(dec,exp.e):>X)::exps
               trColumns pars decs exps args  CE' rest
            | Param m when Types.det T = R && mode=MQuery->
              let par = X.Parameter(typeof<obj>,cn)
              let v = let (colmap,cols) = knowDTO.[tn] 
                      let i = colmap.[cn]
                      cols.[i]
              let CE' = CE.Add(cn,(W,R,new XExp<obj>(par):>XVar,pars.Length)) //TBR
              let args = v::args
              let pars = par::pars
              trColumns pars decs exps args  CE' rest
            | Param m when Types.det T <> R || mode = MSampler -> 
              let par = X.Parameter((typeOf T).MakeByRefType(),cn)
              let exp = X.Assign(par,(trModel TE CE (XExp<int>(X.Constant(-1)):>XExp) m).e)
              let CE' = CE.Add(cn,(W,Types.det T,xexpOf T par,List.length pars))
              trColumns (par::pars)  decs (exp :> X ::exps) ((null:>obj)::args)  CE' rest
            | Input ->
              let indices,vs = mkArray cn T
              assert (indices.Length = length)
              assert (vs.Length = length )
              let par = X.Parameter((typeOf T).MakeArrayType(),cn)
              let CE' = CE.Add(cn,(Y,D,(xexpOf T par).MakeArray(),pars.Length))
              trColumns (par::pars) decs exps (vs:>obj::args) CE' rest
            | Observable m 
            | Latent m when Types.det T = R && mode = MQuery->  
              let vs = let (colmap, rows) = distDTO.[tn] 
                       let a = rows :> obj :?> obj[][]
                       let i = colmap.[cn]
                       Array.init length (fun r -> a.[r].[i])
              let par =  X.Parameter(typeof<obj[]>,cn)
              let CE' = CE.Add(cn,(Y,R,new XExp<obj[]>(par):>XVar,pars.Length))
              trColumns (par::pars) decs exps (vs:>obj::args)  CE' rest
            | Observable m 
            | Latent m when Types.det T <> R || mode = MSampler -> 
              let Ts = (typeOf T).MakeArrayType()
              let par =  X.Parameter(Ts.MakeByRefType(),cn)
              assert par.IsByRef 
              let exit = X.Label(typeof<System.Void>)
              let cont = X.Label(typeof<System.Void>)
              let a = X.Parameter(Ts,cn)
              let xv = X.Variable(typeof<int>,cn+"_i")
              let exp =
               X.Block(
                [| a;xv |],
                [  X.Assign(xv,X.Constant(0)) :> X;
                   X.Assign(a,X.NewArrayBounds(typeOf T,[|X.Constant(length):>X|])) :> X;
                   X.Loop(X.IfThenElse(
                            X.GreaterThanOrEqual(xv,X.ArrayLength(a) :> X),
                            X.Break(exit),
                            X.Block(X.Assign(X.ArrayAccess(a,xv),
                                             (trModel TE  CE (new XExp<int>(xv:>X)) m).e),
                                    X.PreIncrementAssign(xv),
                                    X.Continue(cont))
                            ),exit,cont) :> X;
                   X.Assign(par,a) :> X;
                ] :> seq<Expression>
               ):> X
              let CE = CE.Add(cn,(Y,Types.det T,(xexpOf T par).MakeArray(),pars.Length))
              trColumns (par::pars) decs (exp::exps) ((null:>obj)::args)  CE rest
              
        trColumns pars decs exps args Map.empty table
#endif


  open TypedDTO


  let rec Sample2 db tables  = 
            trTables2 MSampler db [] [] [] [] (Map.empty,[]) tables
            
  and  Query2 db tables = 
            trTables2  MQuery db [] [] [] [] (Map.empty,[]) tables
            
  and trTables2 mode (db:DataBase) 
            pars decs exps args
            ((TE,LTE): Map<TableName,(int*Map<ColumnName, Syntax.B * Syntax.D * XVar * index >)> * List<TableName * List<(ColumnName * (obj-> ColValue )) >>)
            tables  = 
      let objConverter = ObjConverter()
      let id2Rep = db |> Map.map(fun k (_,_,rep,keyToPos) -> rep) // build me up incrementally
      let id2KeyToPos = db |> Map.map(fun k (_,_,_,keyToPos) -> keyToPos) // build me up incrementally
      let trTables =  trTables2 mode (db:DataBase) 
      match tables with
      | [] -> let lam = X.Lambda(X.Block(List.rev decs,List.rev exps),List.rev pars)
              let del = lam.Compile()
              let args = List.toArray(List.rev args)
              let inargs = Array.copy args
              let res = del.DynamicInvoke(args)
              let outargs = args
              let data = LTE |> List.map (fun (tn, cl) ->  let (size_tn,colmap,idRep, keyToPos) = db.[tn]
                                                           let colValues = cl |> List.map  (fun (cn, f) -> let (_,_,_,index) =  (snd TE.[tn]).[cn] in cn, f(outargs.[index]) )  
                                                                              |> Map.ofList
                                                           tn, (size_tn, colValues,idRep, keyToPos)
                                                           ) 
                             |> Map.ofList
              data  : DataBase                                                          

      | (Declaration(Table(tn,_),table)::tables) -> 
        let size = db.[tn]
        let (length, colmap,idRep,idToPos) = db.[tn]
        let rec trColumns pars decs exps args (CE,LCE) columns  =
          match columns with
          |  [] -> 
            trTables pars decs exps args (TE.Add(tn,(length,CE)), (tn,LCE)::LTE) tables
          | (cn,{Type=T;Markup=m})::rest ->
            if mode = MSampler && Types.det T = Qry 
            then trColumns pars decs exps args (CE,LCE) rest
            else
            match m with
            | Hyper e ->
               let dec = X.Parameter(typeOf T,cn)
               let exp = trE mode TE CE (XExp<int>(X.Constant(-1)):>XExp) e
               let CE' = CE.Add(cn,(H,D,xexpOf T (dec:>X),-1))
               let LCE' = LCE
               let decs = dec::decs
               let exps = (X.Assign(dec,exp.e):>X)::exps
               trColumns pars decs exps args  (CE',LCE') rest

            | Param m when Types.det T = R && mode=MQuery->
              let par = X.Parameter(typeof<obj>,cn)
              let colValue = colmap.[cn] :?> Static
              let v = colValue.Value()
              let CE' = CE.Add(cn,(W,R,new XExp<obj>(par):>XVar,pars.Length)) //TBR
              let LCE' = (cn, fun _ ->  colValue :> ColValue)::LCE
              let args = v::args
              let pars = par::pars
              trColumns pars decs exps args  (CE',LCE') rest

            | Param m when Types.det T <> R || mode = MSampler -> 
              let par = X.Parameter((typeOf T).MakeByRefType(),cn)
              let exp = X.Assign(par,(trModel mode TE CE (XExp<int>(X.Constant(-1)):>XExp) m).e)
              let CE' = CE.Add(cn,(W,Types.det T,xexpOf T par,List.length pars))
              let LCE' = ( (cn, fun v -> Static<obj>(v) :> ColValue))::LCE
              trColumns (par::pars)  decs (exp :> X ::exps) ((null:>obj)::args)  (CE',LCE') rest

            | Input ->
              let inputValue = colmap.[cn] :?> TypedDTO.Instance
              let vs = inputValue.get_NonNullValues
              let par = X.Parameter((typeOf T).MakeArrayType(),cn)
              let CE' = CE.Add(cn,(Y,D,(xexpOf T par).MakeArray(),pars.Length))
              let LCE' = ( (cn, fun _ -> inputValue :> ColValue))::LCE
              trColumns (par::pars) decs exps (vs:>obj::args) (CE',LCE') rest

            | Observable m 
            | Latent m when Types.det T = R && mode = MQuery->  
              let vs = let a = colmap.[cn] :?> TypedDTO.Instance
                       [| for v in a.get_NonNullValues -> box v |]
              let par =  X.Parameter(typeof<obj[]>,cn)
              let CE' = CE.Add(cn,(Y,R,new XExp<obj[]>(par):>XVar,pars.Length))
              let LCE' = ((cn, fun v -> let oa = box v :?> obj[]
                                        DistributionInstance<obj> (oa) :> Instance :> ColValue))::LCE
              trColumns (par::pars) decs exps (vs:>obj::args)  (CE',LCE') rest

            | Observable m 
            | Latent m when Types.det T <> R || mode = MSampler -> 
              let Ts = (typeOf T).MakeArrayType()
              let par =  X.Parameter(Ts.MakeByRefType(),cn)
              assert par.IsByRef 
              let exit = X.Label(typeof<System.Void>)
              let cont = X.Label(typeof<System.Void>)
              let a = X.Parameter(Ts,cn)
              let xv = X.Variable(typeof<int>,cn+"_i")
              let exp =
               X.Block(
                [| a;xv |],
                [  X.Assign(xv,X.Constant(0)) :> X;
                   X.Assign(a,X.NewArrayBounds(typeOf T,[|X.Constant(length):>X|])) :> X;
                   X.Loop(X.IfThenElse(
                            X.GreaterThanOrEqual(xv,X.ArrayLength(a) :> X),
                            X.Break(exit),
                            X.Block(X.Assign(X.ArrayAccess(a,xv),
                                             (trModel mode TE CE (new XExp<int>(xv:>X)) m).e),
                                    X.PreIncrementAssign(xv),
                                    X.Continue(cont))
                            ),exit,cont) :> X;
                   X.Assign(par,a) :> X;
                ] :> seq<Expression>
               ):> X
              let CE' = CE.Add(cn,(Y,Types.det T,(xexpOf T par).MakeArray(),pars.Length))
              let rep = TypedDTO.rep (id2Rep, id2KeyToPos) T //TBR
              let mkNonNullableInstance (vs:obj) : Instance  = 
                   (rep).Visit( { new RepVisitor<Instance> with 
                          member this.CaseIntRep                 r  = NonNullableInstance<int        > (vs :?> int[] ):> Instance
                          member this.CaseArrayRep<'T>(r:ArrayRep<'T>)  = NonNullableInstance<'T[]       > (vs :?> 'T[][] ):> Instance
                          member this.CaseUpToRep                r  = NonNullableInstance<int        > (vs :?> int[] ):> Instance
                          member this.CaseUpToSizeRep            r  = NonNullableInstance<int        > (vs :?> int[] ):> Instance
                          member this.CaseBoolRep                r  = NonNullableInstance<bool       > (vs :?> bool[] ):> Instance
                          member this.CaseStringRep              r  = NonNullableInstance<string     > (vs :?> string[] ):> Instance
                          member this.CaseRealRep                r  = NonNullableInstance<real       > (vs :?> real[] ):> Instance
                          member this.CaseGenericIComparableRep  r  = NonNullableInstance<System.IComparable> (vs :?> System.IComparable[] ):> Instance
                          member this.CaseGenericToStringRep     r  = NonNullableInstance<System.IComparable> (vs :?> System.IComparable[] ):> Instance
                          member this.CaseVectorRep     r  = NonNullableInstance<Vector> (vs :?> Vector[] ):> Instance
                          member this.CaseMatrixRep     r  = NonNullableInstance<Matrix> (vs :?> Matrix[] ):> Instance

                    })                                                                               


              let LCE' = (cn, fun vs -> mkNonNullableInstance  vs:> ColValue)::LCE
              trColumns (par::pars) decs (exp::exps) ((null:>obj)::args)  (CE',LCE') rest
        trColumns pars decs exps args (Map.empty,[]) table
    
#endif