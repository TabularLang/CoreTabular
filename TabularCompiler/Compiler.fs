

namespace MicrosoftResearch.Infer.Tabular


[<AutoOpen>]
module Utils =

  type Microsoft.FSharp.Collections.Map<'K, 'V when 'K : comparison > with 
      member t.Keys = t |> Seq.map(fun e -> e.Key)


module Compiler  =
    
  open System.Reflection
  module T = Syntax
  open Syntax
  module Tabular = Syntax
  open Target
  open Ranges
  open Translate

  open MicrosoftResearch.Infer
  open MicrosoftResearch.Infer.Factors


  let verbose = ref true
  let timing = ref true
  let time header f x =
    if (!timing || !verbose) then printfn "(Fun %s" header
    let s = new System.Diagnostics.Stopwatch()
    s.Start()
    try 
      f x 
    finally 
      s.Stop()
      if (!timing || !verbose) then printfn "Fun %s time was %o ms)" header s.ElapsedMilliseconds


  open Microsoft.FSharp.Quotations
  open Microsoft.FSharp.Quotations.DerivedPatterns

  let rec private getMethodFromExpr : Expr -> System.Reflection.MethodInfo = function
  | Lambdas (_ , e') -> getMethodFromExpr e'
  | Microsoft.FSharp.Quotations.Patterns.Call(_, m, _) -> m
  | e -> failwith "cannot extract method info from " e

  // custom factors 
  let dBF = System.Delegate.CreateDelegate(typeof<FactorMethod<double,double,double>>,getMethodFromExpr <@MicrosoftResearch.Infer.Factors.Damp.Backward<float>(0.0,0.0)@>)
            :?> FactorMethod<double,double,double>

  //let fm = new FactorMethod<double,double,double>(MicrosoftResearch.Infer.Factors.Damp.Backward<double>)
  let dampBackwardFactor : MicrosoftResearch.Infer.Models.Variable<float> * MicrosoftResearch.Infer.Models.Variable<float> -> MicrosoftResearch.Infer.Models.Variable<float> = 
        fun (v1,v2) -> 
          MicrosoftResearch.Infer.Models.Variable<double>.Factor<double, double>(dBF,v1,v2)

 
  open MicrosoftResearch.Infer
  open MicrosoftResearch.Infer.Distributions
  open MicrosoftResearch.Infer.Models
  
  let getMethodInfo f = 
    match f with 
        | Quotations.Patterns.Call(_,minfo,_) -> minfo.GetGenericMethodDefinition() 
        | _ -> failwith "fail"
  let rec collapseType : ColumnType -> System.Type = function
    | T_Array (t,e) -> let t' = collapseType t in t'.MakeArrayType()
    | T_Int -> typeof<int>
    | T_Upto(_) -> typeof<int>
    | T_Bool -> typeof<bool>
    | T_Real -> typeof<double>
    | T_String -> typeof<string>
    | T_Link t -> typeof<int>
    | T_Vector -> typeof<Maths.Vector>
    | T_PositiveDefiniteMatrix -> typeof<Maths.PositiveDefiniteMatrix>
    | _ -> failwith "collapseType: only array and simple types expected"

  let newVariableArrayInfo = getMethodInfo <@ Variable.Array<int>(new Range(1)) @> 
  let newVariableArray (t:System.Type) (r:Range): Variable = 
      (newVariableArrayInfo.MakeGenericMethod [| t |]).Invoke(null, [| r |]) :?> Variable

  let variableNewInfo = getMethodInfo <@ Variable.New<int>() @> 
  let variableNew (t:System.Type): Variable = 
      (variableNewInfo.MakeGenericMethod [| t |]).Invoke(null,[| |]) :?> Variable
 
  let variableCutInfo = getMethodInfo <@ Variable.Cut<int>(null:Variable<int>) @> 
  let variableCut (v:Variable) : Variable = 
      (variableCutInfo.MakeGenericMethod [| v.GetDomainType() |]).Invoke(null,[| v |]) :?> Variable
 

  let variableCopyInfo = getMethodInfo <@ Variable.Copy<int>(Variable.New<int>()) @> 
  let variableCopy (v:Variable) = 
   (variableCopyInfo.MakeGenericMethod [| v.GetDomainType() |]).Invoke(null, [| v |]) :?> Variable

 
  let variableArray0CopyInfo = getMethodInfo <@ Variable.Copy<int>(Variable.Array<int>(new Range(0))) @> 
  let variableArray0Copy (v:Variable) = 
      (variableArray0CopyInfo.MakeGenericMethod [| v.GetDomainType().GetElementType() |]).Invoke(null, [| v |]) :?> Variable

  let variableArray1CopyInfo = getMethodInfo <@ Variable.Copy<int>(null:VariableArray<VariableArray<int>,int[][]>) @> 
  let variableArray1Copy (v:Variable) = 
      let tys = [| v.GetDomainType().GetElementType().GetElementType()|]
      (variableArray1CopyInfo.MakeGenericMethod tys).Invoke(null, [| v |]) :?> Variable
  
  let variableArray2CopyInfo = getMethodInfo <@ Variable.Copy<VariableArray<int>,int>(null) @> 
  let variableArray2Copy (v:Variable) = 
      let ty = v.GetType()
      let tys = ty.GetGenericArguments()
      let tys0 = tys.[0].GetGenericArguments()
      (variableArray2CopyInfo.MakeGenericMethod tys0).Invoke(null, [| v |]) :?> Variable
  
  let variableArrayCopy (v:Variable) =
      match v.GetDomainType().GetElementType() with
      | t when not t.IsArray-> variableArray0Copy(v)
      | t when not (t.GetElementType().IsArray) -> variableArray1Copy(v)
      | _ -> variableArray2Copy(v)

  let newVariableArrayWithPrototypeInfo = 
    getMethodInfo <@ Variable.Array<VariableArray<int>, int[][]>(Variable.Array<int>(new Range(1)), new Range(1)) @> 
  let newVariableArrayWithPrototype (t:System.Type) (proto:obj) (r:Range) = 
    (newVariableArrayWithPrototypeInfo.MakeGenericMethod [| proto.GetType(); t |]).Invoke(null, [| proto; r |]) :?> Variable
 
  let initializeToF (dist: IDistribution<'T>, v: Variable<'T>) = 
    v.InitialiseTo(dist)
  let initializeToInfo = getMethodInfo <@ initializeToF(Discrete.Uniform(1), Variable.New<int>()) @> 
  let initializeTo (dist: obj) (v: Variable) = 
    (initializeToInfo.MakeGenericMethod [| v.GetDomainType() |]).Invoke(null, [| dist; v |]) :?> Variable


  type Variable with
     member v.named (name:string) =
          try 
            ignore(v.GetType().GetMethod("Named", [| typeof<string>|]).Invoke(v, [| name |]))
            v
          with 
          | _ -> v

     member v.copy() = 
       match v :> obj with
         | :? IVariableArray -> 
                variableArrayCopy(v)
         | _ -> variableCopy(v)

     member v.setTo(w:Variable) = 
       match (v , w) with
       |  (:? Variable<int> as v),(:? Variable<int> as w) ->
          v.SetTo(w)
       |  (:? Variable<bool> as v),(:? Variable<bool> as w) ->
          v.SetTo(w)
       |  (:? Variable<double> as v),(:? Variable<double> as w) ->
          v.SetTo(w)
       | _ ->
          let _ = v.GetType().GetMethod("SetTo", [|w.GetType()|]).Invoke(v, [| w |]) 
          ()

     
     member v.getObservedValue() =
            v.GetType().GetMethod("get_ObservedValue", [||]).Invoke(v,[||])
          
     member v.observeValue(w:obj) =
       match (v , w) with
       |  (:? Variable<int> as v),(:? int as w) ->
          v.ObservedValue <- w
       |  (:? Variable<bool> as v),(:? bool as w) ->
          v.ObservedValue  <- w
       |  (:? Variable<double> as v),(:? double as w) ->
          v.ObservedValue  <- w
       |  (v,w) when w.GetType().IsArray ->
          let _ = v.GetType().GetMethod("set_ObservedValue", [| w.GetType()|]).Invoke(v, [| w |])
          ()
     member v.subarray(w:Variable<int[]>) =
       match (v,w) with
       |  (v,(:? VariableArray<int> as w)) when v.GetDomainType().IsArray ->
          let t = v.GetDomainType().GetElementType()
          let mi = getMethodFromExpr <@ MicrosoftResearch.Infer.Models.Variable.Subarray((null:VariableArray<int>), (null:VariableArray<int>)) @>
          let mi = mi.GetGenericMethodDefinition().MakeGenericMethod([| t |])
          let v = mi.Invoke(null,  [|v; w|] ) :?> Variable
          v.AddAttribute(new MarginalPrototype(null)) // this is a workaround for a bug in Infer.NET 2.5
          v
     
     member va.assign(r:Range,w:Variable) =
       match (va,w) with
       |  (:? VariableArray<int> as v),(:? Variable<int> as w) ->
          v.[r] <- w
       |  (:? VariableArray<bool> as v),(:? Variable<bool> as w) ->
         v.[r] <- w
       |  (:? VariableArray<double> as v),(:? Variable<double> as w) ->
         v.[r] <- w
       |  (:? VariableArray<Maths.Vector> as v),(:? Variable<Maths.Vector> as w) ->
         v.[r] <- w
       |  (:? VariableArray<string> as v),(:? Variable<string> as w) ->
         v.[r] <- w
       | (v,w) ->
            let ms = v.GetType().GetProperties()
            let m = Array.find (fun (m:PropertyInfo) -> m.Name = "Item" && 
                                                        m.GetIndexParameters().[0].ParameterType = typeof<Range>) ms
            let r =  m.SetValue(v,w, [| r |])
            ()
      member va.assignindex(idx:Variable,w:Variable) =
       let i = idx :?> Variable<int>
       match (va,w) with
       |  (:? VariableArray<int> as v),(:? Variable<int> as w) ->
          v.[i] <- w
       |  (:? VariableArray<bool> as v),(:? Variable<bool> as w) ->
         v.[i] <- w
       |  (:? VariableArray<double> as v),(:? Variable<double> as w) ->
         v.[i] <- w
       |  (:? VariableArray<Maths.Vector> as v),(:? Variable<Maths.Vector> as w) ->
         v.[i] <- w
       |  (:? VariableArray<string> as v),(:? Variable<string> as w) ->
         v.[i] <- w
       | (v,w) ->
            let ms = v.GetType().GetProperties()
            let m = Array.find (fun (m:PropertyInfo) ->  m.Name = "Item" && 
                                                         m.GetIndexParameters().[0].ParameterType = typeof<Variable<int>>) ms
            let r =  m.SetValue(v,w, [| i |])
            ()
      


  let interp_Gt (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         Variable.op_GreaterThan(v,w) :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         Variable.op_GreaterThan(v,w) :> Variable
  let interp_GtEq (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         Variable.op_GreaterThanOrEqual(v,w) :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         Variable.op_GreaterThanOrEqual(v,w) :> Variable
  let interp_Lt (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         Variable.op_LessThan(v,w) :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         Variable.op_LessThan(v,w) :> Variable
  let interp_LtEq (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         Variable.op_LessThanOrEqual(v,w) :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         Variable.op_LessThanOrEqual(v,w) :> Variable
  let interp_Plus (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
        Variable.op_Addition(v,w) :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         Variable.op_Addition(v,w) :> Variable
  let interp_Minus (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
        Variable.op_Subtraction(v,w) :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         Variable.op_Subtraction(v,w) :> Variable
  let interp_Eq (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         Variable.op_Equality(v,w) :> Variable
      | (:? Variable<bool> as v),(:? Variable<bool> as w) ->
         Variable.op_Equality(v,w) :> Variable
  let interp_Neq (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         Variable.op_Inequality(v,w) :> Variable
      | (:? Variable<bool> as v),(:? Variable<bool> as w) ->
         Variable.op_Inequality(v,w) :> Variable
  let interp_And(v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<bool> as v),(:? Variable<bool> as w) ->
         Variable.op_BitwiseAnd(v,w) :> Variable
  let interp_Or (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<bool> as v),(:? Variable<bool> as w) ->
         Variable.op_BitwiseOr(v,w) :> Variable
  let interp_Not (v:Variable) = 
      match v with
      | (:? Variable<bool> as v)->
         Variable.op_LogicalNot(v) :> Variable
  let interp_Negate (v:Variable) = 
      match v with
      | (:? Variable<double> as v)->
         Variable.op_UnaryNegation(v):>Variable
  let interp_Mult (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         Variable.op_Multiply(v,w) :> Variable     
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         Variable.op_Multiply(v,w) :> Variable 
  let interp_Max (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         Variable.Max(v,w) :> Variable      
  let interp_Div (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         Variable.op_Division(v,w) :> Variable     
  let interp_Mod (v:Variable,w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         Variable.op_Modulus(v,w) :> Variable     


  let setAnyValueRange (dst:Variable) (src:Variable) =
      match dst.GetValueRange(false),src.GetValueRange(false) with
        | null,null -> ()
        | null, r -> 
           dst.SetValueRange(r)
        | r,r' -> 
           if r = r' then () else failwith "inconsistent value range"

  let rec interpPrim (RE:Map<r,Range>)
                 (VE:Map<v,Variable>)
                 (AE:Map<v,Variable>) p es : Variable = 
        let interp = interpE RE VE AE
        match (p,es) with
        | Prim.Gt,[e1;e2] -> interp_Gt (interp e1, interp e2)
        | Prim.Lt,[e1;e2] -> interp_Lt (interp e1, interp e2)
        | Prim.GtEq,[e1;e2] -> interp_GtEq (interp e1, interp e2)
        | Prim.LtEq,[e1;e2] -> interp_LtEq (interp e1, interp e2)
        | Prim.Eq,[e1;e2]  -> 
              interp_Eq (interp e1, interp e2)
        | Prim.Neq,[e1;e2]  -> 
              interp_Neq (interp e1, interp e2)
        | Prim.Mult,[e1;e2]  -> 
              interp_Mult (interp e1, interp e2)
        | Prim.Div,[e1;e2]  -> 
              interp_Div (interp e1, interp e2)
        | Prim.Mod,[e1;e2]  -> 
              interp_Mod (interp e1, interp e2)
        | Prim.Max,[e1;e2]  -> 
              interp_Max (interp e1, interp e2)
        | Prim.Minus,[e1;e2]  -> 
              interp_Minus (interp e1, interp e2)
        | Prim.Negate,[e1]  -> 
              interp_Negate (interp e1)
        | Prim.Not,[e1]  -> 
              interp_Not (interp e1)
        | Prim.Plus,[e1;e2]  -> 
              interp_Plus (interp e1, interp e2)
        | Prim.And,[e1;e2]  -> interp_And(interp e1, interp e2)
        | Prim.Or,[e1;e2]  -> interp_Or (interp e1, interp e2)
        | Prim.Factor(FactorName"DampBackward"),[e1;e2] -> 
           dampBackwardFactor( interp e1 :?> Variable<double> , 
                               interp e2 :?> Variable<double>) :> Variable
        | Prim.Factor(FactorName"Logistic"),[e1] ->  
           match (interp e1) with
           |  (:? Variable<double> as v1) ->
                Variable.Logistic(v1) :> Variable
        | Prim.Factor(FactorName"Probit"),[e1;e2] ->  
           match (interp e1,interp e2) with
           |  (:? Variable<double> as v1),
              (:? Variable<double> as v2) ->
                Variable.op_GreaterThan(Variable<double>.GaussianFromMeanAndPrecision(v1,v2),0.0) :> Variable
        | Prim.Factor(FactorName"Sum"),[e1] ->  
           match (interp e1) with
           |  (:? VariableArray<double> as v1) ->
                Variable.Sum(v1) :> Variable
        | Prim.Factor(FactorName "Softmax"),[e1] ->  
           match (interp e1) with
           |  (:? VariableArray<double> as v1) ->
                Variable.Softmax(v1) :> Variable
        | Prim.Factor(FactorName"InnerProduct"),[e1;e2] ->  
           match (interp e1,interp e2) with
           |  (:? Variable<Maths.Vector> as v1),
              (:? Variable<Maths.Vector> as v2) ->
                Variable.InnerProduct(v1,v2) :> Variable
        | Prim.Factor(FactorName"VectorFromArray"),[e1] ->  
           match (interp e1) with
           |  (:? Variable<double[]> as v1) ->
                Variable.Vector(v1) :> Variable
        | Prim.Factor(FactorName"Exp"),[e1] ->  
           match (interp e1) with
           |  (:? Variable<double> as v1) ->
                Variable.Exp(v1) :> Variable
        | Prim.Factor(FactorName"Log"),[e1] ->  
           match (interp e1) with
           |  (:? Variable<double> as v1) ->
                Variable.Log(v1) :> Variable
        | Prim.Factor(FactorName"Subarray"),[e1;e2] ->  
           match (interp e1),(interp e2) with
           |  v1,(:? Variable<int[]> as v2) ->
                v1.subarray(v2) 
        | Prim.Factor(FactorName "DiagonalPDMatrix"),[e1] ->  //failwith "NYI: DiagonalMatrix"
           match (interp e1) with
           |  (:? VariableArray<double> as v1) when v1.IsObserved  ->
                let mat = Maths.PositiveDefiniteMatrix.Identity(v1.ObservedValue.GetLength(0))
                mat.SetDiagonal(Maths.Vector.FromArray(v1.ObservedValue)) |> ignore
                Variable.Constant<Maths.PositiveDefiniteMatrix>(mat):> Variable
           | _ -> failwith "NFI: DiagonalPDMatrix"            
        | Prim.Factor(FactorName "IdentityScaledBy"),[e1;e2]->  
            match (interp e1,interp e2) with
            |  (:? Variable<int> as v1),
               (:? Variable<double> as v2) when ( v1.IsObserved && v2.IsObserved) ->
                Variable.Constant<Maths.PositiveDefiniteMatrix>(Maths.PositiveDefiniteMatrix.IdentityScaledBy(v1.ObservedValue,v2.ObservedValue)) :> Variable
            | _ -> failwith "NFI: IdentityScaledBy"
       (*
        | Prim.Factor(FactorName "GetItems"),[e1;e2]->  
            let v = interp e1
            let w = interp e2
            let m = typeof<Variable>.GetGenericMethod("GetItems", [|  v.GetType().GetGenericArguments().[0]|])
            let _ =  m.Invoke(v, [| i; w |])
            ()
        | Prim.Factor(FactorName "SubArray"),[e1;e2]-> 
            let v = interp e1
            let w = interp e2
            let m = typeof<Variable>.GetGenericMethod("GetItems", [|  v.GetType().GetGenericArguments().[0]|])
            let _ =  m.Invoke(v, [| i; w |])
            ()
        *)
        | Prim.Factor(FactorName "#Cut"),[e]  -> 
            let v = interp e
            variableCut v
            
        | Prim.Factor(FactorName f),es when f.StartsWith("#") -> 
            let vs = [| for e in es -> interp e :> obj|]
            let methods = typeof<Variable>.GetMethods()
            typeof<Variable>.GetMethod(f.Substring(1),[| for v in vs -> v.GetType() |]).Invoke(null,vs)
            :?> Variable
        | Prim.Factor(FactorName f),es -> failwithf "interpPrim %A not yet implemented" f

  and interpDist (RE:Map<r,Range>)
                 (VE:Map<v,Variable>)
                 (AE:Map<v,Variable>) d es : Variable = 
        let interp = interpE RE VE AE
        match (d,es) with
        | T.DiscreteUniform, [ e1] -> 
          Variable.DiscreteUniform(interp e1 :?> Variable<int>) :> Variable
        | T.GaussianFromMeanAndPrecision,[e1;e2] -> 
           Variable.GaussianFromMeanAndPrecision(interp e1 :?> Variable<double>, interp e2 :?> Variable<double>) :> Variable
        | T.GaussianFromMeanAndVariance,[e1;e2] -> 
           Variable.GaussianFromMeanAndVariance(interp e1 :?> Variable<double>, interp e2 :?> Variable<double>) :> Variable
        | T.VectorGaussianFromMeanAndPrecision,[e1;e2] -> 
           Variable.VectorGaussianFromMeanAndPrecision(interp e1 :?> Variable<Maths.Vector>, interp e2 :?> Variable<Maths.PositiveDefiniteMatrix>) :> Variable
        | T.VectorGaussianFromMeanAndVariance,[e1;e2] -> 
           Variable.VectorGaussianFromMeanAndVariance(interp e1 :?> Variable<Maths.Vector>, interp e2 :?> Variable<Maths.PositiveDefiniteMatrix>) :> Variable
        | T.GammaFromShapeAndRate,[e1;e2] ->  
           Variable.GammaFromShapeAndRate(interp e1 :?> Variable<double>, interp e2 :?> Variable<double>) :> Variable 
        | T.GammaFromMeanAndVariance,[e1;e2] ->  
           Variable.GammaFromMeanAndVariance(interp e1 :?> Variable<double>, interp e2 :?> Variable<double>) :> Variable 
        | T.GammaFromShapeAndScale,[e1;e2] ->  
           Variable.GammaFromShapeAndScale(interp e1 :?> Variable<double>, interp e2 :?> Variable<double>) :> Variable 
        | T.Bernoulli, [e1]  -> 
           Variable.Bernoulli(interp e1 :?> Variable<float>) :> Variable
        | T.Beta, [e1;e2] ->  
           Variable.Beta(interp e1 :?> Variable<double>, interp e2 :?> Variable<double>) :> Variable 
        | T.Binomial, [e1;e2] ->
           Variable.Binomial(interp e1 :?> Variable<int>, interp e2 :?> Variable<float>) :> Variable
        | T.Discrete, [Rng r;e1] ->
           let v1 = interp e1:?>Variable<Maths.Vector>
           Variable.Discrete((* RE.[r],*) v1)
           :> Variable
        | T.Dirichlet, [Rng r;e1] ->
           let v1 = interp e1
           Variable.Dirichlet( RE.[r],Variable.Vector(v1:?>Variable<float[]>)) :> Variable
        | T.DirichletUniform, [Rng r] ->
           Variable.DirichletSymmetric( RE.[r],Variable.Constant(1.0)) :> Variable
        | T.DirichletSymmetric, [Rng r;e1] ->
           let v1 = interp e1
           Variable.DirichletSymmetric( RE.[r],v1:?>Variable<float>) :> Variable
        | T.Poisson, [e1] ->
            Variable.Poisson(interp e1 :?> Variable<float>) :> Variable
        | T.WishartFromShapeAndScale, [e1;e2] -> 
            Variable.WishartFromShapeAndScale(interp e1 :?> Variable<float>,interp e2 :?> Variable<Maths.PositiveDefiniteMatrix>) :> Variable
        | T.WishartFromShapeAndRate, [e1;e2] -> 
            Variable.WishartFromShapeAndRate(interp e1 :?> Variable<float>,interp e2 :?> Variable<Maths.PositiveDefiniteMatrix>) :> Variable

  //TODO: merge AE and VE
  and interpE (RE:Map<r,Range>)
                 (VE:Map<v,Variable>)
                 (AE:Map<v,Variable>) e : Variable =
        let interp = interpE RE VE AE
        match e with
        | Var v when VE.ContainsKey v-> VE.[v]
        | Var v when AE.ContainsKey v-> AE.[v]
        | Const (IntConst i) -> Variable.Constant<int>(i) :> Variable
        | Const (BoolConst b) -> Variable.Constant<bool>(b) :> Variable
        | Const (RealConst r) ->  Variable.Constant<double>(r) :> Variable
        | Const (StringConst s) ->  Variable.Constant<string>(s) :> Variable
        | Prim(p,es) -> interpPrim RE VE AE p es
        | Dist(d,es) ->  interpDist RE VE AE d es
        | IndexRng (e1,rv) -> 
           match (interp e1,RE.[rv]) with
           |  v1, r -> 
            v1.GetType().GetMethod("get_Item", [| typeof<Range> |]).Invoke(v1, [| r |])  :?> Variable
        | Index (e1,e2) -> 
           match (interp e1,interp e2) with
           |  v1,(:?Variable<int> as v2) -> 
             v1.GetType().GetMethod("get_Item", [| typeof<Variable<int>> |]).Invoke(v1, [| v2 |]) :?> Variable 
        | InitialiseTo(e1,d) ->
            let v1 = interp e1 
            initializeTo d v1

  let isArray t = 
      match t with 
      | T_Array _ -> true 
      | _ -> false  
  
  let rec VariableNew (RE:Map<r,Range>)  t =
          match t with
          | T_Int -> Variable.New<int>() :> Variable
          | T_Upto (TypedExp(T.Const (IntConst n),_)) -> 
               let v = Variable.New<int>() :> Variable
               v.SetValueRange(RE.[rangeOf (RConst n,0)])
               v
          | T_Upto (TypedExp(T.SizeOf tn,_)) -> 
               let v = Variable.New<int>() :> Variable
               v.SetValueRange(RE.[rangeOf (RSizeOf tn,0)])
               v
          | T_Bool -> Variable.New<bool>() :> Variable
          | T_Real -> Variable.New<real>() :> Variable
          | T_String -> Variable.New<string>() :> Variable
          | T_Vector ->Variable.New<Maths.Vector>() :> Variable
          | T_PositiveDefiniteMatrix ->Variable.New<Maths.PositiveDefiniteMatrix>() :> Variable
          | T_Link t -> 
               let v = Variable.New<int>():> Variable
               v.SetValueRange(RE.[range(t)])
               v
          | T_Array (u, e) when not(isArray(u))-> 
               let r = match e with 
                           | TypedExp(T.Const(IntConst n),_)-> rangeOf (RConst n,depth t) 
                           | TypedExp(T.SizeOf tn,_)-> rangeOf(RSizeOf tn,depth t)
                           | _ -> failwithf "BUG: array type with %A with  non-constant size" t 
               let ve = VariableNew RE u 
               let va = newVariableArray (collapseType u) RE.[r] 
               setAnyValueRange va ve
               va
          | T_Array (u,e) when isArray(u)-> 
               let r = match e with 
                           | TypedExp(T.Const(IntConst n),_)-> rangeOf (RConst n,depth t) 
                           | TypedExp(T.SizeOf tn,_)-> rangeOf(RSizeOf tn,depth t)
                           | _ -> failwithf "BUG: array type with %A with  non-constant size" t 
               let ve = VariableNew RE u 
               let va = newVariableArrayWithPrototype (collapseType t) ve RE.[r] 
               setAnyValueRange va ve
               va
          
  
  let rec VariableArray t (RE:Map<r,Range>) (r:Range)  =
          match t with
          | T_Int -> Variable.Array<int>(r):> Variable
          | T_Upto (TypedExp(T.Const (IntConst i),_)) -> 
              let v = Variable.Array<int>(r):> Variable
              v.SetValueRange(RE.[rangeOf (RConst i,0)])
              v
           | T_Upto (TypedExp(T.SizeOf tn,_)) -> 
              let v = Variable.Array<int>(r):> Variable
              v.SetValueRange(RE.[rangeOf(RSizeOf tn,0)])
              v
          | T_Bool -> Variable.Array<bool>(r) :> Variable
          | T_Real -> Variable.Array<real>(r) :> Variable
          | T_String -> Variable.Array<string>(r) :> Variable
          | T_Vector ->Variable.Array<Maths.Vector>(r) :> Variable
          | T_PositiveDefiniteMatrix ->Variable.Array<Maths.PositiveDefiniteMatrix>(r) :> Variable
          | T_Upto (TypedExp(T.SizeOf t,_))  
          | T_Link t -> 
               let v = Variable.Array<int>(r):> Variable
               v.SetValueRange(RE.[rangeOf (RSizeOf t,0)])
               v
          | T_Array (u,_) -> 
              let ve = VariableNew RE t
              let va = newVariableArrayWithPrototype ((collapseType t).MakeArrayType()) ve r 
              setAnyValueRange va ve
              va
         

          



  let rec interpS (RE:Map<r,Range>)
                  (VE:Map<v,Variable>)
                  (AE:Map<v,Variable>) s = 
       match s with
       | CloneRng(s,r) ->
          let rng = RE.[r].Clone()
          let rng = rng.Named(s)
          (RE.Add(s,rng),VE,AE) 
       | LetRng (r,i) -> 
          let rng = new Range((VE.[i]):?> Variable<int>)
          let rng = rng.Named(r)
          (RE.Add(r,rng),VE,AE) 
       | LetVar (v,e) -> 
          let ve = (interpE RE VE AE e).named v
          (RE,VE.Add(v,ve),AE) 
       | LetNew (v,t) ->
          let ve = (VariableNew RE t).named v
          (RE,VE.Add(v,ve),AE) 
       | LetArray (v,r,t) -> 
          let ve = (VariableArray t RE (RE.[r])).named v // add type
          (RE,VE,AE.Add(v,ve)) 
       | ObserveValue(v,t,o) ->
          let ve = if AE.ContainsKey v  then AE.[v] else VE.[v] // TODO: merge AE and VE
          ve.observeValue(o)
          (RE,VE,AE) 
       | Assign (v,r,E) -> 
          let ve = interpE RE VE AE E
          let va = if AE.ContainsKey v then AE.[v] else VE.[v] // TODO: merge AE and VE
          va.assign(RE.[r],ve)
          (RE,VE,AE) 
       | AssignIndex (v,E0,E1) ->
          let v0 = interpE RE VE AE E0
          let v1 = interpE RE VE AE E1
          let va = if AE.ContainsKey v then AE.[v] else VE.[v] // TODO: merge AE and VE
          va.assignindex(v0,v1)
          (RE,VE,AE) 
       |  SetTo(v,E) ->
          let ve = interpE RE VE AE E
          VE.[v].setTo(ve)
          (RE,VE,AE) 
       |  Seq (S1,S2) -> 
          let (RE,VE,AE) = interpS RE VE AE S1
          interpS RE VE AE S2
       |  ForEach(r,S) ->
          use foreachBlock = Variable.ForEach(RE.[r])
          let _ = interpS RE VE AE S
          (RE,VE,AE)
        | ForLoop(r,x,S) ->
          use foreachBlock = Variable.ForEach(RE.[r])
          let VE' = VE.Add(x,foreachBlock.Index)
          let _ = interpS RE VE' AE S
          (RE,VE,AE)
       (*
       |  ForEach(r,S) ->
          let rng = RE.[r]
          let rng' = rng.Clone()
          use ifBlock = Variable.ForEach(rng')
          let _ = interpS (RE.Add(r,rng')) VE AE S
          
          (RE,VE,AE)
          *)
       |  IfNot(v,S) ->
          let v = VE.[v]
          use ifnotBlock = Variable.IfNot(v :?> Variable<bool>)
          let _ = interpS RE VE AE S
          (RE,VE,AE)  
       |  If(v,S) ->
          let v = VE.[v]
          use ifBlock = Variable.If(v :?> Variable<bool>)
          let _ = interpS RE VE AE S
          (RE,VE,AE) 
       |  Skip -> 
          (RE,VE,AE)
       |  LetCopy(v,E) ->
          let ve = interpE RE VE AE E
          let ve' = ve.copy() 
          (RE,VE.Add(v,ve'),AE) 
      (*
       |  Switch(v,S) ->
          let v = VE.[v]
          use SwitchBlock = Variable.Switch(v :?> Variable<int>)
          let _ = interpS RE VE AE S
          (RE,VE,AE)
           
       *)
        |  Switch(v,S) ->
          let v = VE.[v]
          let SwitchBlock = Variable.Switch(v :?> Variable<int>)
          let _ = interpS RE VE AE S
          SwitchBlock.CloseBlock()
          (RE,VE,AE) 
        | SetValueRange(v,r) ->
          let v = VE.[v]
          v.SetValueRange(RE.[r])
          (RE,VE,AE) 
  
 


  let interpM s =
      let e = Variable.Bernoulli(0.5)
      let b = Variable.If(e)
      let (RE,VE,AE) = interpS Map.empty Map.empty Map.empty s
      b.CloseBlock()
      (e,(RE,VE,AE))
