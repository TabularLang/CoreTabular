namespace MicrosoftResearch.Infer.Tabular
//
//module LatentLearner = 
//  open System.Reflection
//
//  open Microsoft.FSharp.Reflection
//  open Microsoft.FSharp.Quotations
//  open Microsoft.FSharp.Quotations.Patterns
//  open Microsoft.FSharp.Quotations.ExprShape
//
//  open MicrosoftResearch.Infer
//  open MicrosoftResearch.Infer.Maths
//  open MicrosoftResearch.Infer.Distributions
//  open MicrosoftResearch.Infer.Fun.Learner
//  module Fun = MicrosoftResearch.Infer.Fun.FSharp.Syntax
//  module FSharpInference = MicrosoftResearch.Infer.Fun.FSharp.Inference
//  module CoreInference = MicrosoftResearch.Infer.Fun.Core.Inference
//  module CoreSyntax = MicrosoftResearch.Infer.Fun.Core.Syntax
//  module CoreTransformations = MicrosoftResearch.Infer.Fun.Core.Transformations
//  
//  
//  type ILatentLearner<'TDistW,'TX,'TY,'TZ,'TDistY, 'TDistZ> = interface
//    abstract TrainPredict: x:'TX * y:'TY -> 'TDistW *  'TDistZ
//    abstract TrainPredictWithLogEvidence:  x:'TX * y:'TY * IAlgorithm option * int option -> double * ('TDistW *  'TDistZ)
//    abstract TrainPredictWithLEAndFit: x:'TX * y:'TY * display:bool -> double * ('TDistW * 'TDistZ *'TDistY)
//  end
//
//
//  let LatentLearner<'TH,'TW,'TX,'TY,'TZ,'TDistW,'TDistY, 'TDistZ when 'TY : equality and 'TZ : equality>
//        (m: Model<'TH,'TW,'TX,'TY*'TZ>, h:'TH) = 
//    { new ILatentLearner<'TDistW,'TX,'TY,'TZ,'TDistY,'TDistZ> with
//
//     (*
//      member l.TrainPredict(x:'TX,y:'TY) =
//          let expr = <@ fun () ->
//                        let w = (%m.Prior) h 
//                        let (y',z) = (%m.Gen) (w,x)
//                        Fun.observe(y=y') //transform TY -> TY option and observe conditionnaly ?
//                        (w,z) @> 
//                        
//         // printfn "y\n%A" y
//         
//          do Fun.Core.Inference.setVerbose(true)
//       //   let e = new InferenceEngine(new VariationalMessagePassing())
//          let e = new InferenceEngine(new ExpectationPropagation())
//          e.ShowMsl  <- true
//          do Fun.Core.Inference.setEngine(e)
//          let (distW:'TDistW),(distZ:'TDistZ) = FSharpInference.infer expr ()
//          //let (distW:'TDistW),(distZ:'TDistZ) = FSharpInference.inferModel (FSharpInference.makeModel(expr)) ()
//         // printfn "dW\n%A" distW
//         // printfn "dZ\n%A" distZ
//       //  let context = FSharpInference.getAssemblyContext() 
//       //   let e = Fun.Reflection.unquote expr
//       //   let (distW,distZ) =  CoreInference.fromCompound<'TDistW*'TDistZ> (CoreInference.infer context e)
//          distW,distZ
//     *)
//
//     member l.TrainPredict(x:'TX,y:'TY) =
//          let expr = <@ fun (x,y) ->
//                        let w = (%m.Prior) h 
//                        let (y',z) = (%m.Gen) (w,x)
//                        Fun.observe(y=y') //transform TY -> TY option and observe conditionnaly ?
//                        (w,z) @> 
//                        
//         // printfn "y\n%A" y
//         
//          //do Fun.Core.Inference.setVerbose(true)
//          //let e = new InferenceEngine(new VariationalMessagePassing())
//          let e = new InferenceEngine(new ExpectationPropagation())
//          //e.ShowMsl  <- true
//          //e.ShowTimings <- true
//          do Fun.Core.Inference.setEngine(e)
//          do Fun.Core.Inference.setTiming(true)
//          let ((distW:'TDistW),(distZ:'TDistZ)) = FSharpInference.infer expr (x,y)
//          //let (distW:'TDistW),(distZ:'TDistZ) = FSharpInference.inferModel (FSharpInference.makeModel(expr)) ()
//         // printfn "dW\n%A" distW
//         // printfn "dZ\n%A" distZ
//       //  let context = FSharpInference.getAssemblyContext() 
//       //   let e = Fun.Reflection.unquote expr
//       //   let (distW,distZ) =  CoreInference.fromCompound<'TDistW*'TDistZ> (CoreInference.infer context e)
//          distW,distZ
//
// 
//     member l.TrainPredictWithLogEvidence(x:'TX,y:'TY, ?algo : IAlgorithm, ?numberOfIterations : int) = //-40000
//          let expr = <@ fun (x,y) ->
//                        let w = (%m.Prior) h 
//                        let (y',z) = (%m.Gen) (w,x)
//                        Fun.observe(y=y') //transform TY -> TY option and observe conditionnaly ?
//                        (w,z) @> 
//                        
//          //let e = new InferenceEngine(new VariationalMessagePassing())
//          let e = new InferenceEngine(defaultArg algo (new ExpectationPropagation() :> _))
//          // e.ShowMsl  <- true
//          e.ShowTimings <- true
//          e.NumberOfIterations <- 10
//          //Rand.Restart(12347)
//          do Fun.Core.Inference.setTiming(true)
//          do Fun.Core.Inference.setEngine(e)
//          // Fun.Core.Inference.setVerbose(true)
//          let evidence, ((distW:'TDistW),(distZ:'TDistZ)) = FSharpInference.inferWithLogEvidence expr (x,y)
//          evidence,(distW,distZ)
//
//     member l.TrainPredictWithLEAndFit(x:'TX,y:'TY,display) =  //-70000
//          let expr = <@ fun (x,y) ->
//                        let w = (%m.Prior) h 
//                        let (y',z) = (%m.Gen) (w,x)
//                        Fun.observe(y=y')
//                        let (yfit,zfit) = (%m.Gen) (w,x)
//                        Fun.observe(z=zfit)
//                        (w,z,yfit) @> 
//                        
//       //   let e = new InferenceEngine(new VariationalMessagePassing())
//          let e = new InferenceEngine(new ExpectationPropagation())
//          if display then 
//            e.ShowMsl  <- true
//            e.ShowTimings <- true
//
//          do Fun.Core.Inference.setEngine(e)
//          Fun.Core.Inference.setVerbose(true)
//
//          let evidence, ((distW:'TDistW),(distZ:'TDistZ), (distY:'TDistY)) = FSharpInference.inferWithLogEvidence expr (x,y)
//          evidence,(distW,distZ,distY)
//
//
//(*
//      member l.TrainPredictWithLogEvidence(x:'TX,y:'TY) =
//          let (distW,distZ) = l.TrainPredict(x,y)
//          let expr =  
//            <@ fun (x,y) ->
//               let b = Fun.random(Fun.Bernoulli(0.5))
//               if b then
//                        let w = (%m.Prior) h 
//                        let (y',z) = (%m.Gen) (w,x)
//                        Fun.observe(y=y') //transform TY -> TY option and observe conditionnaly ?
//               b @> 
//                        
//       //   let e = new InferenceEngine(new VariationalMessagePassing())
//          let e = new InferenceEngine(new ExpectationPropagation())
//          e.ShowMsl  <- true
//          e.ShowTimings <- true
//          do Fun.Core.Inference.setEngine(e)
//          let evidence:Bernoulli = FSharpInference.infer expr (x,y)
//          evidence.LogOdds,(distW,distZ)
//          *)
//    }
//  let LearnerFromModel(m) = LatentLearner(m,m.HyperParameter)
//
// 