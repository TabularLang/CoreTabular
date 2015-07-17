#if INTERACTIVE
#I @"bin\Debug" // wherever the .dll files are located
#r @"Infer.Runtime.dll";
#r @"Infer.Compiler.dll";
#r @"Infer.Fun.dll";
#r @"DataLayer.dll";
#r @"MarkupFunLayer.dll";
#else
module tests 
#endif

//open System.Collections.Generic
//open MicrosoftResearch.Infer.Fun
//open MicrosoftResearch.Infer.Fun.Learner
//open MicrosoftResearch.Infer.Fun.FSharp.Syntax
//open MicrosoftResearch.Infer
//open MicrosoftResearch.Infer.Fun.Learner
//open MicrosoftResearch.Infer.Distributions
//open MicrosoftResearch.Infer.Maths // Access to Vector and PositiveDefiniteMatrix, etc.
//open Microsoft.FSharp.Reflection
//open MicrosoftResearch.Infer.Tabular
//open MicrosoftResearch.Infer.Tabular.Tabular
//open MicrosoftResearch.Infer.Tabular.DataLayer
////open MicrosoftResearch.Infer.Tabular.DataLayer.IO
////open MicrosoftResearch.Infer.Tabular.OldService
//
////open MicrosoftResearch.Infer.Fun.FunDBLayer
//
//
////You might need to increase stack size.
////"C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\Tools\vsvars32.bat" editbin /STACK:100000000  "C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\IDE\CommonExtensions\Microsoft\TestWindow\vstest.executionengine.x86.exe"
//
//type D<'T> = IDistribution<'T>
//let integers = Seq.unfold(fun i -> Some(i, i+1)) 0
//module S = Fun.FSharp.Syntax
//module FArray = Microsoft.FSharp.Collections.Array
//
////
////[<Fact>]
////let queryTrueskills () =    
////    let loader = new DAOLoader(__SOURCE_DIRECTORY__ + "\paper-trueskills-query.accdb")
////    let concreteData, posToID = readTable loader TrueSkillQuery.Schema
////    let (evidence, DistDTO predictedPZ, knowledgeDW, Some (DistDTO impliedPY)) = performInferenceWithFit(concreteData, TrueSkillQuery.Schema, true) |> Async.RunSynchronously
////
////    ()
////
////[<Fact>]
////let mooc () =    
////    let loader = new DAOLoader(__SOURCE_DIRECTORY__ + "\mooc.accdb")
////    let concreteData, posToID = readTable loader DifficultyAbility.Schema
////    let (evidence, DistDTO predictedPZ, knowledgeDW, _) = performInference(concreteData, DifficultyAbility.Schema) |> Async.RunSynchronously
////
////    printfn "logLikelihood Correct %A" evidence
////
////    ()
////
////
////   
////
////let moocEvaluation (schema:Database<Markup>) =  
////    printfn "evaluating %A" schema.Name  
////    let loader = new DAOLoader(__SOURCE_DIRECTORY__ + "\mooc - evaluation.accdb")
////   
////    let concreteData, posToID = readTable loader schema
////    let (e,DistDTO predictedPZ, knowledgeDW,_) = performInference(concreteData, schema) |> Async.RunSynchronously
////
////    printfn "log Evidence %A" e
////
////    let (predictedcolMap, predicteds) = predictedPZ.["Responses"]
////    
////    let (actualcolMap, actuals) = concreteData.["Responses"]
////    // eval on training data
////    if predictedcolMap.ContainsKey "answerIdxP" then 
////      let mutable logLikelihoodTraining = 0.
////      let mutable logLikelihoodTest = 0.
////      let mutable countTraining=0
////      let mutable countTest=0
////      for actual, predict in Seq.zip actuals predicteds do
////        if (actual.[actualcolMap.["Training"]] :?> bool)
////        then
////          countTraining <- countTraining+1
////          let answer = predict.[predictedcolMap.["answerIdxP"]] :?> Discrete  
////          logLikelihoodTraining <- logLikelihoodTraining + answer.GetLogProb(actual.[actualcolMap.["answerIdx"]] :?> int)
////          ()
////        else
////          countTest <- countTest+1
////          let answer = predict.[predictedcolMap.["answerIdxP"]] :?> Discrete
////          logLikelihoodTest <- logLikelihoodTest + answer.GetLogProb(actual.[actualcolMap.["answerIdx"]] :?> int)
////          ()
////      printfn "logLikelihood AnswerIdx %A (training)" (logLikelihoodTraining / (float)countTraining)
////      printfn "logLikelihood AnswerIdx %A (test)" (logLikelihoodTest / (float)countTest)
////
////[<Fact>]
////let moocDAUEvaluation () = moocEvaluation(DifficultyAbilityUntrainedDiscreteEvaluation.Schema)   
////
////[<Fact>]
////let moocDAEvaluation () = moocEvaluation(DifficultyAbilityDiscreteEvaluation.Schema)   
////    
////[<Fact>]
////let moocDASEvaluation () = moocEvaluation(DifficultyAbilitySkipDiscreteEvaluation.Schema)   
////  
//////[<Fact>]
//////let moocDASCEvaluation () = moocEvaluation(DifficultyAbilitySkipCheatDiscreteEvaluation.Schema)   
////   
////[<Fact>]
////let moocDASGEvaluation () = moocEvaluation(DifficultyAbilitySkipGuessDiscreteEvaluation.Schema)   
//// 
////[<Fact>]
////let moocDASGCEvaluation () = moocEvaluation(DifficultyAbilitySkipGuessCheatDiscreteEvaluation.Schema)   
//// 
//   
//    
//
//
////[<Fact>]
////let predictionForGaussian () =    
////    let loader = new DAOLoader(__SOURCE_DIRECTORY__ + "\test-gauss.accdb")
////    let dicDatas, posToID = readTable loader ConjugateGaussian.Schema
////    let (evidence, DistDTO predictedPZ, knowledgeDW, fitPY) = performInferenceWithFit(dicDatas, ConjugateGaussian.Schema, true) |> Async.RunSynchronously
////    
////    let ( responsecolMap, responses) = dicDatas.["sample"]
////
////    let mutable logLikelihood = 0.
////    printfn "%A" logLikelihood
////    ()
////    
////
////[<Fact>]
////let testDifficultyAbility () =    
////    let sizes = [("Students",15);("Answers",6*10+1);("Questions",10);("Responses",15*10);]|> Map.ofList
////   // let sizes = [("Students",423);("Answers",6*50+1);("Questions",50);("Responses",21150);]|> Map.ofList
////    let dStudent   = sizes.["Students"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
////    let dQuestions = sizes.["Questions"] |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
////    let dAnswers   = sizes.["Answers"]   |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
////    let x = ((((ONE,
////                [| for i in 1..sizes.["Students"]  -> ONE |]),
////                [| for i in 1..sizes.["Answers"]   -> (ONE,"text") |]),
////           //     [| for i in 1..sizes.["Questions"] -> ((((((( (ONE,"question text"), dAnswers.Sample()),
////           //                                                                       0),1),2),3),4),5) |]),
////                [| for i in 1..sizes.["Questions"] -> 
////                        let answer =  (new Distributions.Discrete([| for i in 1..6 -> 1.0/(float) 6 |])).Sample()
////                        let offset =  (i-1) * 6
////                        
////                        ((((((( (ONE,"question text"), offset + answer),
////                                                       offset + 0),offset + 1), offset+2), offset+3), offset+4), offset+5)|]),
////                 [| for s in 1..sizes.["Students"] -> [| for q in 1..sizes.["Questions"] -> ((ONE,s-1),q-1) |] |]|> FArray.concat
////             )
////
////    let schema = DifficultyAbility.Schema
////    let latentModel = trDB schema sizes
////    
////    let data = latentModel.Sample (x)
////    let data2 = (latentModel.PackX x).merge data
////
////    let storer  = new DAOLoader(__SOURCE_DIRECTORY__ + "\GeneratedMooc.accdb") :> IStorer
////    reCreate storer (DifficultyAbility.Schema |> Database.ConcreteSubset |> Database.ItemsTSNameTable) (DTO data2)
////    let b = latentModel.Test(x)
////
////    Assert.True(true)
//
//  
//let selftestStudentTeacher schema (numStudents,numQuestions) =    
//    let numAnswers=6
//    let sizes = [("Students",numStudents);
//                 ("Questions",numQuestions);
//                 ("Answers",1+6*numQuestions)
//                 ("Responses",numStudents*numQuestions)
//                ]|> Map.ofList
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//     
//    let x = ((((ONE,
//                [| for i in 1..sizes.["Students"]  -> ONE |]),
//                [| for i in 1..sizes.["Answers"]   -> (ONE,"text") |]),
//                [| for i in 1..sizes.["Questions"] -> 
//                        let answer =  (new Distributions.Discrete([| for i in 1..6 -> 1.0/(float) 6 |])).Sample()
//                        let offset = 1 + (i-1) * 6  
//                        ((((((( (ONE,"question text"), offset + answer),
//                                                       offset + 0),offset + 1), offset+2), offset+3), offset+4), offset+5)|]),
//                [| for s in 1..sizes.["Students"] -> [| for q in 1..sizes.["Questions"] -> ((ONE,s-1),q-1) |] |]|> FArray.concat
//             )
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//[<Fact>]
//let selftestDifficultyAbility () = 
//    selftestStudentTeacher DifficultyAbility.Schema (15,10)
//
//[<Fact>]
//let selftestDifficultyAbilitySkip () =    
//    selftestStudentTeacher DifficultyAbilitySkip.Schema (15,10)
//   
//
//[<Fact>]
//let selftestDifficultyAbilitySkipCheat () =    
//    selftestStudentTeacher DifficultyAbilitySkipCheat.Schema (15,10)
//    
//
//  
//let selftestStudentTeacherDiscreteUniform schema (numStudents,numQuestions) =    
//    let numAnswers=6
//    let sizes = [("Students",numStudents);
//                 ("Questions",numQuestions);
//                 ("ResponsesTrain",numStudents*numQuestions)
//                ]|> Map.ofList
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    ///(((Boolean * Boolean[]) * ((Boolean * Int32) * Int32)[]) * ((Boolean * Int32) * Int32)[])
//     
//    let x = ((((ONE,
//                [| for i in 1..sizes.["Students"]  -> ONE |]),
//                [| for i in 1..sizes.["Questions"] -> 
//                        let answer =  (new Distributions.Discrete([| for i in 1..6 -> 1.0/(float) 6 |])).Sample()
//                        (ONE,answer) |]),
//                [| for s in 1..sizes.["Students"] -> [| for q in 1..sizes.["Questions"] -> ((ONE,s-1),q-1) |] |]|> FArray.concat
//             ))
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//[<Fact>]
//let selftestDifficultyAbilityDiscreteUniform () = 
//    selftestStudentTeacherDiscreteUniform DifficultyAbilityDiscrete.Schema (200,50)
//
//
//
//
//
//
//let evalStudentTeacher  (numStudents,numQuestions) =  
//    let schema = DifficultyAbilityEvaluation.Schema  
//    let tables = schema.Tables
//    let numAnswers=6
//    let sizes = [("Students",numStudents);
//                 ("Questions",numQuestions);
//                 ("Answers",1+6*numQuestions)
//                 ("ResponsesTraining",numStudents*(numQuestions/2))
//                 ("ResponsesTrue",numStudents*(numQuestions-(numQuestions/2)))
//                ]|> Map.ofList
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//   
//    let x = ((((ONE,
//                [| for i in 1..sizes.["Students"]  -> ONE |]),
//                [| for i in 1..sizes.["Answers"]   -> (ONE,"text") |]),
//                [| for i in 1..sizes.["Questions"] -> 
//                        let answer =  (new Distributions.Discrete([| for i in 1..6 -> 1.0/(float) 6 |])).Sample()
//                        let offset = 1 + (i-1) * 6  
//                        ((((((( (ONE,"question text"), offset + answer),
//                                                       offset + 0),offset + 1), offset+2), offset+3), offset+4), offset+5)|]),
//                [| for s in 1..sizes.["Students"] -> [| for q in 1..sizes.["Questions"]/2-> ((ONE,s-1),q-1) |] |]|> FArray.concat),
//                [| for s in 1..sizes.["Students"] -> [| for q in (sizes.["Questions"]/2+1)..sizes.["Questions"] -> ((ONE,s-1),q-1) |] |]|> FArray.concat
//            
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//[<Fact>]
//let selftestGaussianWithPrior()  =    
//    let schema = GaussianWithPrior.Schema
//   // let tables = GenGaussian.Schema.Tables
//    let numAnswers=6
//    let sizes = [("Sample",10000);
//                ]|> Map.ofList
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//     
//    let x = (ONE,
//                [| for i in 1..sizes.["Sample"]  -> ONE |])
//             
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//
//(*
//[<Fact>]
//let selftestInfernoClassicMM () =    
//    let tables = InfernoClassicMM.Schema.Tables
//    let numRecords=100
//    let sizes = [("InfernoClassicMM",numRecords);
//                 ("X0",numRecords);
//                 ("X1",numRecords)
//                 ("X2",numRecords)
//                ]|> Map.ofList
//
//    let model = trDB tables sizes
//    printfn "%s" (model.ToString())
//    let samplers = [| for z in 0 .. 2 -> 
//                      [|
//                        for x in 0 .. 2 -> 
//                        Distributions.Gaussian.FromMeanAndPrecision((float)(z * x * 100),1.0)
//                      |]
//                   |]
//    let zD = Distributions.Discrete.Uniform(3)
//    let zs = [| for i in 1..sizes.["InfernoClassicMM"]  -> zD.Sample() |]
//    let x = ((((ONE,
//                [| for i in 1..sizes.["InfernoClassicMM"]  -> ONE |]),
//                [| for i in 1..sizes.["X0"]   -> ((ONE,i-1),samplers.[zs.[i-1]].[0].Sample()) |]),
//                [| for i in 1..sizes.["X1"]   -> ((ONE,i-1),samplers.[zs.[i-1]].[1].Sample()) |]),
//                [| for i in 1..sizes.["X2"]   -> ((ONE,i-1),samplers.[zs.[i-1]].[2].Sample())|])
//
//    let b = model.Test(x)
//    Assert.True(b)
//*)
//[<Fact>]
//let selftestInfernoClassicMM () = 
//    //TBC this seems to require explicit symmetry breaking and vmp   
//    let schema = InfernoClassicMM.Schema
//    let numRecords=100
//    let sizes = [("InfernoClassicMM",numRecords);
//                 ("X0",numRecords/3);
//                 ("X1",numRecords/3)
//                 ("X2",numRecords/3)
//                ]|> Map.ofList
//
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//   
//    let x = ((((ONE,
//                [| for i in 1..sizes.["InfernoClassicMM"]  -> ONE |]),
//                [| for i in 1..sizes.["X0"]   -> ((ONE,(i-1)*3)) |]),
//                [| for i in 1..sizes.["X1"]   -> ((ONE,(i-1)*3+1)) |]),
//                [| for i in 1..sizes.["X2"]   -> ((ONE,(i-1)*3+2))|])
//
//    let wIC = ((((ONE,
//                  Vector.FromArray([|1000.0/3.0;1000.0/3.0;100.0/3.0|])),
//                  [| for i in 1 .. 3 -> ((float) (i* 10),1.0) |]),
//                  [| for i in 3 .. 6 -> ((float) (i* 10),1.0) |]),
//                  [| for i in 6 .. 9 -> ((float) (i* 10),1.0) |]) 
//    let wX0 =  (ONE,())
//    let wX1 =  (ONE,())
//    let wX2 =  (ONE,())
//    let w = ((((ONE,wIC),wX0),wX1),wX2)
//      
//        
//
//   //((((Boolean * ((((Boolean * Vector) * (Double * Double)[]) * (Double * Double)[]) * (Double * Double)[])) * (Boolean * unit)) * (Boolean * unit)) * (Boolean * unit))
//        
//    let b = model.TestWithParameter(w,x)
//    Assert.True(b)
//
////
////[<Fact>]
////let testDAREQuery () =    
////    let schema = DAREQuery.Participant
////   // let numParticipants = 120
////   // let numQuestions = 60
////    let numParticipants = 121
////    let numQuestions = 60
////    let answerRange = DARE.answerRange
////  
//// 
////    //let dParticipant   = new Distributions.Discrete([| for i in 1..numParticipants -> 1.0/(float) numParticipants |])
////    //let dQuestions = new Distributions.Discrete([| for i in 1..numQuestions -> 1.0/(float) numQuestions |])
////    //let dAnswers   = new Distributions.Discrete([| for i in 1..answerRange-> 1.0/(float) answerRange |])
////    let dTrain = new Distributions.Bernoulli(0.7)
////    
////    let Participants = [| for i in 1..numParticipants  -> ONE |]
////  
////    let questionsGT = [| for i in 1..numQuestions -> 
////                         let dummyTrueAnswer = 0
////                         let training = dTrain.Sample()
////                         ((ONE,dummyTrueAnswer),training) |]
////    let questions =  questionsGT // [| for (_,training) in questionsGT -> (ONE,training) |]
////    let questionsTR = let training = 
////                          questionsGT |> Array.mapi (fun i r -> (i,r)) 
////                                      |> Array.filter (fun (i,(r,training)) -> training)  
////                      [| for (i,((_,_),_)) in training -> (ONE,i) |]
////              
////    let responsesGT = [| for s in 1..numParticipants -> 
////                         [| for q in 1..numQuestions -> 
////                            let dummyTrueAnswer = 0
////                            let training = dTrain.Sample()
////                            ((((ONE,s-1),q-1),training),dummyTrueAnswer) |] |]|> FArray.concat
////    let responses =  responsesGT // [| for (((_,s),q),_) in responsesGT -> ((ONE,s),q) |]
////    let responsesTR = let training = 
////                          responsesGT |> Array.mapi (fun i r -> (i,r)) 
////                                      |> Array.filter (fun (i,((r,training),_)) -> training)  
////                      [| for (i,((((_,s),q),_),_)) in training -> (ONE,i) |]
////    
////    let x = (((((ONE,
////                 Participants),  
////                 questions),
////                 questionsTR),
////                 responses),
////                 responsesTR)    
////             
////    let sizes = [("Participants",Participants.Length);
////                 ("Questions",questions.Length); 
////                 ("QuestionsTR",questionsTR.Length);
////                 ("Responses",responses.Length);
////                 ("ResponsesTR",responsesTR.Length);]|> Map.ofList
////    let latentModel = trDB schema sizes
////    
////    let data = latentModel.Sample (x)
////    let packed = (latentModel.PackX x)
////    let data2 = packed.merge data
////
////    let storer  = new DAOLoader(__SOURCE_DIRECTORY__ + "\GeneratedParticipant.accdb") :> IStorer
////    reCreate storer (schema |> Database.ConcreteSubset |> Database.ItemsTSNameTable) (DTO data2)
////    let b = latentModel.Test(x)
////
////    Assert.True(true)
////
////let dareEvaluation (schema:Database<Markup>) =  
////    printfn "evaluating %A" schema.Name
////
////    //printfn "%O" (Tabular.dataBaseToTex schema)
////
////    // HACK FUN to avoid generating switches
////    MicrosoftResearch.Infer.Fun.FSharp.Inference.noSwitchHack := true;  
////    
////    let loader = new DAOLoader(__SOURCE_DIRECTORY__ + "\DARE.accdb")
////   
////    let concreteData, posToID = readTable loader schema
////    Rand.Restart(12347)
////
////    let (e,DistDTO predictedPZ, knowledgeDW,_) = performInference(concreteData, schema) |> Async.RunSynchronously
////    
////    MicrosoftResearch.Infer.Fun.FSharp.Inference.noSwitchHack := false;  
////    
////    let report = fun  table ->
////      let (predictedcolMap, predicteds) = predictedPZ.[table]
////    
////      let (actualcolMap, actuals) = concreteData.[table]
////      // eval on training data
////      if predictedcolMap.ContainsKey "Answer" then 
////        let mutable logProbTest = 0.
////    
////        let mutable countTest=0
////        let mutable numCorrect=0;
////        for actual, predict in Seq.zip actuals predicteds do
////          if (not(actual.[actualcolMap.["Training"]] :?> bool)) 
////          then
////            countTest <- countTest+1
////            let answer = predict.[predictedcolMap.["Answer"]] :?> Discrete
////            let trueAnswer =  actual.[actualcolMap.["TrueAnswer"]] :?> int
////            let mode = answer.GetMode()
////            if (mode = trueAnswer) then numCorrect <- numCorrect+1
////            logProbTest <- logProbTest + answer.GetLogProb(trueAnswer)
////        printfn "Test set size %A" countTest
////        printfn "Test set accuracy %.2F (percent)" (100.0* ((float) numCorrect/ (float) countTest))
////        printfn "Avg log prob TrueAnswer %.3F (test)" (logProbTest / (float)countTest)
////        printfn "Avg prob TrueAnswer %.3f (test)" (System.Math.Exp(logProbTest / (float)countTest))
////      else()
////    // report "ResponsesTest"
////    report "Responses"
////    report "Questions"
////    printfn "Model Evidence %A" e
////   
////[<Fact>]
////let dareProgressionEvaluation () = 
////    dareEvaluation(DAREEval.Ability) 
////    printfn "\n---------------------"
////    dareEvaluation(DAREEval.DA) 
////    printfn "\n---------------------"
////    dareEvaluation(DAREEval.DARE) 
//
//
//
//
////[<Fact>]
////let dareQuestionEvaluation () = dareEvaluation(DAREEval.Question)  
//
////[<Fact>]
////let dareDAREEvaluation () = dareEvaluation(DAREEval.DARE)  