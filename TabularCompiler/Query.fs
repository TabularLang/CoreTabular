namespace MicrosoftResearch.Infer.Tabular

#if DEAD
open MicrosoftResearch.Infer
open MicrosoftResearch.Infer.Models
open MicrosoftResearch.Infer.Distributions
open MicrosoftResearch.Infer.Collections
//open MicrosoftResearch.Infer.Tabular.Service
open Syntax
module FArray = Microsoft.FSharp.Collections.Array

module Query = 
  type value = obj
  // these abbreviations are a hack to enable easy cut-n-past from the compiler code
  type Variable = obj
  type Variable<'T> = 'T
  
  let inline interp_Gt (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         v>w :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         v>w :> Variable
  let inline interp_GtEq (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         v>=w :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         v>=w :> Variable
  let inline interp_Lt (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
        v < w :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
        v < w :> Variable
  let inline interp_LtEq (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
         v <= w :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
          v <= w :> Variable
  let inline interp_Plus (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
        v+w :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
        v+w :> Variable
  let inline interp_Minus (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
        v-w :> Variable
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
        v-w :> Variable
  let inline interp_Eq (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
        v = w :> Variable
      | (:? Variable<bool> as v),(:? Variable<bool> as w) ->
         v=w :> Variable
  let inline interp_Neq (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
        v<>w :> Variable
      | (:? Variable<bool> as v),(:? Variable<bool> as w) ->
        v<>w :> Variable
  let inline interp_And(v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<bool> as v),(:? Variable<bool> as w) ->
         (v && w) :> Variable
  let inline interp_Or (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<bool> as v),(:? Variable<bool> as w) ->
         (v|| w) :> Variable
  let inline interp_Not (v:Variable) = 
      match v with
      | (:? Variable<bool> as v)->
         not v :> Variable
  let inline interp_Negate (v:Variable) = 
      match v with
      | (:? Variable<double> as v)->
         (-v) :>Variable
  let inline interp_Mult (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         v * w :> Variable     
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
        v+w:> Variable 
  let inline interp_Max (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         max v w :> Variable      
  let inline interp_Div (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<double> as v),(:? Variable<double> as w) ->
         v / w :> Variable 
   
  let inline interp_Mod (v:Variable) (w:Variable) = 
      match v,w with
      | (:? Variable<int> as v),(:? Variable<int> as w) ->
        v % w:> Variable     
  let interp_Logistic (v:Variable) = 
      match v with
      | (:? Variable<real> as v) ->
         exp(v) / (1.0+exp(v)) :> Variable  
             
  let rec interpPrim p es : Variable = 

        //TBC with missing primitives
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
        | Prim.And,[e1;e2]  -> interp_And e1 e2
              //if e1 :?> Variable<bool> then e2 else false :> Variable 
              
        | Prim.Or,[e1;e2]  -> interp_Or e1 e2
             // if e1 :?> Variable<bool> then e2 else false :> Variable 
        | Prim.Factor(FactorName"DampBackward"),[e1;e2] -> 
           e1
        | Prim.Factor(FactorName"Logistic"),[e1] -> 
           interp_Logistic e1
      (* TBC
        | Prim.Factor(FactorName"Probit"),[e1;e2] ->  
           match (e1,e2) with
           |  (:? Variable<double> as v1),
              (:? Variable<double> as v2) ->
                Variable.op_GreaterThan(Variable<double>.GaussianFromMeanAndPrecision(v1,v2),0.0) :> Variable
                *)
        | Prim.Factor(FactorName"Sum"),[e1] ->  
           match (e1) with
           |  (:? Variable<obj[]> as v1) ->
                let mutable sum:double = 0.0
                for i = 0 to v1.Length-1 do
                  sum <- sum + (v1.[i] :?> double)
                sum :> Variable
       (*
        | Prim.Factor(FactorName "Softmax"),[e1] ->  
           match (e1) with
           |  (:? VariableArray<double> as v1) ->
                Variable.Softmax(v1) :> Variable
                *)
        | Prim.Factor(FactorName"InnerProduct"),[e1;e2] ->  
           match (e1,e2) with
           |  (:? Variable<Maths.Vector> as v1),
              (:? Variable<Maths.Vector> as v2) ->
                Maths.Vector.InnerProduct(v1,v2):> Variable
        | Prim.Factor(FactorName"VectorFromArray"),[e1] ->  
           match (e1) with
           |  (:? Variable<obj[]> as v1) ->
                let ds = Array.init v1.Length (fun i ->  (v1.[i]):?>double)  //TBR
                Maths.Vector.FromArray(ds) :> Variable
        | Prim.Factor(FactorName"Exp"),[e1] ->  
           match (e1) with
           |  (:? Variable<double> as v1) ->
               System.Math.Exp(v1) :> Variable
        | Prim.Factor(FactorName"Log"),[e1] ->  
           match (e1) with
           |  (:? Variable<double> as v1) ->
               System.Math.Log(v1) :> Variable
        | Prim.Factor(FactorName "DiagonalPDMatrix"),[e1] ->  //failwith "NYI: DiagonalMatrix"
           match ( e1) with
           |  (:? Variable<obj[]> as v1)   ->
                let mat = Maths.PositiveDefiniteMatrix.Identity(v1.GetLength(0))
                let da = Array.init v1.Length (fun i -> v1.[i] :?> double)
                mat.SetDiagonal(Maths.Vector.FromArray(da)) |> ignore
                Variable.Constant<Maths.PositiveDefiniteMatrix>(mat):> Variable

        | Prim.Factor(FactorName "IdentityScaledBy"),[e1;e2]->  // I doubt this will work...
            match (e1,e2) with
            |  (:? Variable<int> as v1),
               (:? Variable<double> as v2) ->
                (Maths.PositiveDefiniteMatrix.IdentityScaledBy(v1,v2)) :> Variable
        
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
           |  (:? Variable<obj[]> as v1) ->
               let rec maxi cm n =
                   if n = v1.Length then cm
                   else let cm' = if (v1.[cm]:?> double) >= (v1.[n] :?> double) then cm else n
                        in maxi cm' (n+1)
               in
                  maxi 0 0 :> obj 
        | Prim.Factor(FactorName "ArgMin"),[e1] ->
          match (e1) with
           |  (:? Variable<obj[]> as v1) ->
               let rec mini cm n =
                   if n = v1.Length then cm
                   else let cm' = if (v1.[cm]:?> double) <= (v1.[n] :?> double) then cm else n
                        in mini cm' (n+1)
               in
                  mini 0 0 :> obj 
        | Prim.Factor(FactorName "#Max"),[e1] ->
          match (e1) with
           |  (:? Variable<obj[]> as v1) ->
               let rec maxi cm n =
                   if n = v1.Length then cm
                   else let cm' = if cm >= (v1.[n] :?> double) then cm else (v1.[n] :?> double)
                        in maxi cm' (n+1)
               in
                  maxi System.Double.NegativeInfinity 0 :> obj 
        | Prim.Factor(FactorName "#Sqrt"),[e1] ->
          match (e1) with
           |  (:? Variable<double> as v1) ->
               System.Math.Sqrt(v1 : double) :> Variable  
        | Prim.Factor(FactorName "#Round"),[e1] ->
          match (e1) with
           |  (:? Variable<double> as v1) ->
               System.Math.Round(v1: double) :> Variable                   
        | Prim.Factor(FactorName f),es -> 
          failwithf "interpPrim %A not yet implemented" f
          

  let tt = true :> Variable
  let ff = false :> Variable
  let rec trE (TE:Map<TableName,(int*Map<ColumnName, Syntax.B * Syntax.D * value>)>) (CE:Map<ColumnName, Syntax.B * Syntax.D * value>) (i:int) et  : value = 
      let (TypedExp(e,t)) = et 
      match e with 
      | Var x -> match CE.[x] with
                 | (H,_,v) -> v
                 | (W,_,v) -> v
                 | (Y,_,vs) -> let a =  vs :?> System.Collections.IList
                               a.[i]
      | Const (IntConst v) -> v :> obj
      | Const (BoolConst v) -> v :> obj
      | Const (RealConst v) -> v :> obj
      | Const (StringConst v) -> v :> obj
      | SizeOf(tn) -> fst (TE.[tn]) :> obj
      | Array es -> [| for e in es -> trE TE CE i e|] :> obj
      | Prim(Prim.And,[e1;e2])  -> 
             if trE TE CE i e1 :?> Variable<bool> then trE TE CE i e2 else ff  
      | Prim(Prim.Or,[e1;e2])  -> 
             if trE TE CE i e1 :?> Variable<bool> then tt  else trE TE CE i e2  
      | Prim(Prim.Factor(FactorName "Sum"),
             [TypedExp(ForLoop(x,e1,e2),_)]) ->
             let v1 = trE TE CE i e1 in
             match v1 with
             | :? int as b ->
             let mutable sum : double = 0.0
             for j  = 0 to b-1 do 
               sum <- sum + (trE TE (CE.Add(x,(W,D,j:>obj))) i e2 :?> double) 
             sum :> obj
      | Prim(p,es) -> interpPrim p (List.map (trE TE CE i) es)
      | Dist(d,es) -> failwith "bug: sampling at query level"
      | Let(x,e1,e2) ->
         let v1 = trE TE CE i e1 in
         trE TE (CE.Add(x,(W,D,v1))) i e2
      | DeRef(e,tn,cn) -> 
         let v = trE TE CE i e in
         match v, (snd(TE.[tn])).[cn] with
         | :? int as k, (_,_,(:? System.Collections.IList as a)) ->
             a.[k]
       | Ref(tn,cn) -> 
         let (_,_,o) = (snd(TE.[tn])).[cn] 
         o
      | If (e1,e2,e3) ->
         let v1 = trE TE CE i e1 in
         match v1 with
         | :? bool as b ->
           if b then trE TE CE i e2 else trE TE CE i e3
      | ForLoop(x,e1,e2) ->
         let v1 = trE TE CE i e1 in
         match v1 with
         | :? int as b ->
           let a = Array.zeroCreate b : obj[]
           for j = 0 to b-1 do
             a.[j] <- trE TE (CE.Add(x,(W,D,j:>obj))) i e2  
           a:>obj
      | Subscript(e1,e2) ->
         let v1 = trE TE CE i e1 in
         let v2 = trE TE CE i e2 in
         match v1, v2 with
         |  (:? System.Array as a),(:? int as k) ->
             a.GetValue(k)
        (*
         |  (:? ConvertibleToArray as a),(:? int as k) ->
             a.ToArray().GetValue(k) // a terrible hack
        *)
         |  (:? System.Collections.Generic.IList<Distributions.Bernoulli> as a),(:? int as k) ->
             a.[k] :> obj
            
         |  (:? ConvertibleToArray as a),(:? int as k) ->
             let indexer = a.GetType().GetProperty("Item")
             indexer.GetValue(v1,[|v2|])
      | Constraint(e1,_) ->
            trE TE CE i e1 
      | Infer((Beta|BetaFromMeanAndVariance),[],("alpha"|"trueCount"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Beta as d ->  d.TrueCount :> obj
      | Infer((Beta|BetaFromMeanAndVariance),[],("beta"|"falseCount"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Beta as d -> d.FalseCount :> obj
      | Infer((Beta|BetaFromMeanAndVariance),[],("Mean"|"mean"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Beta as d -> d.GetMean() :> obj
      | Infer((Beta|BetaFromMeanAndVariance),[],("Variance"|"variance"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Beta as d -> d.GetVariance() :> obj
      | Infer(Bernoulli,[],"Bias",e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Bernoulli as d -> d.GetMean() :> obj
      | Infer(GaussianFromMeanAndVariance,[],("Mean"|"mean"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Gaussian as d -> d.GetMean() :> obj
      | Infer(GaussianFromMeanAndVariance,[],("Variance"|"variance"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Gaussian as d -> d.GetVariance() :> obj
      | Infer(GaussianFromMeanAndPrecision,[],("Mean"|"mean"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Gaussian as d -> d.GetMean() :> obj
      | Infer(GaussianFromMeanAndPrecision,[],("Precision"|"precision"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Gaussian as d -> d.Precision :> obj
      | Infer(Dist.Dirichlet,[e0],"counts",e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Dirichlet as d ->
           let counts = d.PseudoCount
           [| for i in 0..counts.Count-1 -> let r = counts.[i] in r :> obj |] :> obj
      | Infer(Dist.Discrete,[e0],"probs",e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Discrete as d ->
           let counts = d.GetProbs()
           [| for i in 0..counts.Count-1 -> let r = counts.[i] in r :> obj |] :> obj
      | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Mode"|"mode"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Discrete as d -> d.GetMode() :> obj
      | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Median"|"median"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Discrete as d -> d.GetMedian() :> obj 
      | Infer((Dist.Discrete|Dist.DiscreteUniform),[e0],("Mean"|"mean"),e1) ->
         let v1 = trE TE CE i e1 
         match v1 with
         | :? Distributions.Discrete as d -> d.GetMean() :> obj    


  let trModel TE CE i m = 
      match m with
      | TypedModel(MExp e,_) ->
        trE TE CE i e
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

  let rec trTables (DTO dto) (DistDTO distDTO) (KnowDTO knowDTO)
            (TE: Map<TableName,(int*Map<ColumnName, Syntax.B * Syntax.D * value>)>)
            tables = 
      let trTables =  trTables (DTO dto) (DistDTO distDTO) (KnowDTO knowDTO)
      match tables with
      | [] -> TE
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
        let rec trColumns CE columns  =
          match columns with
          |  [] -> 
            trTables (TE.Add(tn,(length,CE))) tables
          | (cn,{Type=T;Markup=m})::rest ->
            match m with
            | Hyper e ->
               let v = trE TE CE (-1) e
               let CE' = CE.Add(cn,(H,D,v))
               trColumns CE' rest
            | Param m when Types.det T = R->
              let v = let (colmap,cols) = knowDTO.[tn] 
                      let i = colmap.[cn]
                      cols.[i]
              let CE' = CE.Add(cn,(W,R,v))
              trColumns CE' rest
            | Param m when Types.det T <> R-> 
              let v = trModel TE CE (-1) m
              let CE' = CE.Add(cn,(W,D,v))
              trColumns CE' rest
            | Input ->
              let indices,vs = mkArray cn T
              //assert (Array.fold )
              assert (indices.Length = length)
              assert (vs.Length = length )
              let CE' = CE.Add(cn,(Y,D,vs:>obj))
              trColumns CE' rest
            | Observable m 
            | Latent m when Types.det T = R ->  
             // let vs = let (colmap,cols) = distDTO.[tn] in Seq.nth(colmap.[cn]) cols //bind the distribution
              let vs = let (colmap, rows) = distDTO.[tn] 
                       let a = rows :> obj :?> obj[][]
                       let i = colmap.[cn]
                       Array.init length (fun r -> a.[r].[i])
                      // [| for r in rows -> r.[i] |] :> obj //bind the distribution
              let CE' = CE.Add(cn,(Y,R,vs :> obj))
              trColumns CE' rest
            | Observable m 
            | Latent m when Types.det T <> R  -> 
              let vs  = //[| for i in 0..length-1 -> trModel TE CE i m|] 
                        Array.init length (fun i -> trModel TE CE i m)
              let CE' = CE.Add(cn,(Y,R,vs :> obj))
              trColumns CE' rest
              
        trColumns Map.empty table

#endif