#if INTERACTIVE
#I @"bin\Debug" // wherever the .dll files are located
#r @"Infer.Runtime.dll";
#r @"Infer.Compiler.dll";
#r @"Infer.Fun.dll";
#r @"DataLayer.dll";
#r @"MarkupFunLayer.dll";
#else
module testsArchived //All the potentially useful test not pertaining to the recommender
#endif

//open Xunit
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
//open MicrosoftResearch.Infer.Tabular.OldService
//open MicrosoftResearch.Infer.Tabular.DataLayer
//
//
////You might need to increase stack size.
////"C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\Tools\vsvars32.bat" editbin /STACK:100000000  "C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\IDE\CommonExtensions\Microsoft\TestWindow\vstest.executionengine.x86.exe"
//
//
//type D<'T> = IDistribution<'T>
//let TABLESIZE = 10
//let sizeMap tables = List.fold (fun (map:Map<string,int>) (name, _) -> map.Add(name,TABLESIZE)) (Map.empty) tables
//let sizeMap' tables = List.fold (fun (map:Map<string,int>) (name, _) -> map.Add(name,if name = "Ratings" then 500 else TABLESIZE)) (Map.empty) tables
//let integers = Seq.unfold(fun i -> Some(i, i+1)) 0
//module S = Fun.FSharp.Syntax
//module FArray = Microsoft.FSharp.Collections.Array
//
//
//
//
//[<Fact>]
//let trueSkillsTest () =    
//    let loader = new DAOLoader(__SOURCE_DIRECTORY__ + "\paper-trueskills.accdb")
//    let dicDatas, posToID = readTable loader TrueSkill.Schema
//    let (evidence, DistDTO predictedPZ, knowledgeDW, _) = performInference(dicDatas, TrueSkill.Schema) |> Async.RunSynchronously
//
//    let ( responsecolMap, responses) = dicDatas.["ResponsesTrain"]
//    let (predictedcolMap, predicted) = predictedPZ.["ResponsesTrain"]
//    
//    let mutable logLikelihood = 0.
//    for response, predict in Seq.zip responses predicted do
//      let know = predict.[predictedcolMap.["know"]] :?> Bernoulli  // = probit( student ability - question difficulty)
//      logLikelihood <- logLikelihood + know.GetLogProb(response.[responsecolMap.["correct"]] :?> bool)
//      ()
//    printfn "%A" logLikelihood
//    ()
//    
//
//
//module DifficultyAbilitySmall =
//  let Schema =
//      {Name = "DifficultyAbilitySmall";
//        Tables = [
//                  "Answers",   {Columns=[]};
//                  "Questions", {Columns=[
//                                         "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answers",    {Type=T_Array(T_Link "Answers",Const 2)   ; Markup=Latent(Gen(Arr [| Column "answer0"; Column "answer1";|]))} ]};
//                  "Responses", {Columns=["questionID", {Type=T_Link "Questions" ; Markup=Input};
//                                          "know"      , {Type=T_Bool             ; Markup=Latent(CBernoulli)  };
//                                          "guess"     , {Type=T_Int              ; Markup=Latent(Gen(DiscreteUniform(Const 2)))};
//                                      //    "answer"    , {Type=T_Link "Answers"   ; Markup=Observable(Gen(Index(Deref("questionID",Column "answers"), Column("guess")))) };
//                                           "answer"    , {Type=T_Link "Answers"   ; Markup=Observable(Gen(StrictIf(Column("know")  , Deref("questionID",Column "answer"), 
//                                                                                                                              Index(Deref("questionID",Column "answers"), Column("guess"))))) };
//                                         ]} ]}
//
//
//[<Fact>]
//let selftestDifficultyAbilitySmall () =    
//    let schema = DifficultyAbilitySmall.Schema
//    let sizes = [
//                 ("Questions",4);
//                 ("Answers",2)
//                 ("Responses",1)] |> Map.ofList   
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dQuestions = sizes.["Questions"] |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dAnswers   = sizes.["Answers"]   |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//
//    let x = ((((ONE,    
//                [| for i in 1..sizes.["Answers"]   -> ONE |]),
//                [| for i in 1..sizes.["Questions"] -> (((ONE, dAnswers.Sample()),
//                                                        0), 
//                                                        1) |]),
//                [| for i in 1..sizes.["Responses"] -> (ONE,dQuestions.Sample()) |]))
//                
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//// simplified with just two answers
//module BinaryDifficultyAbility =
//  let Schema =
//      {Name = "DifficultyAbility";
//        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                      ; Markup=Latent(CGaussian)} ]}
//                  //  "Answers",   {Columns=["text"      , {Type=T_String           ; Markup=Input}; ]};
//                  "Questions", {Columns=["text"      , {Type=T_String                    ; Markup=Input};
//                                         (*
//                                         "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer2"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer3"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer4"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer5"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         *)
//                                         "answer"    , {Type=T_Bool            ; Markup=Input};
//                                         "difficulty", {Type=T_Real                       ; Markup=Latent(CGaussian)};         
//                                        // "answers",    {Type=T_Array(T_Link "Answers")   ; Markup=Latent(Gen(Arr [| Column "answer0" ; Column "answer1"; Column "answer2"; Column "answer3"; Column "answer4"; Column "answer5" |]))} 
//                                        ]};
//                  "Responses", {Columns=["studentID" , {Type=T_Link "Students"  ; Markup=Input};
//                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
//                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty")),RealConst 1.0))) };
//                                          "guess"     , {Type=T_Bool             ; Markup=Latent(Gen(Bernoulli(RealConst(0.5))))};
//                                          "answer"    , {Type=T_Bool              ; Markup=Observable(Gen(If(Column("know"), Deref("questionID",Column "answer"), 
//                                                                                                                             Column("guess"))
//                                                                                                            )) };
//                                          "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer")))) }; ]} ]}
//
//[<Fact>]
//let selftestBinaryDifficultyAbility () =    
//    let schema = BinaryDifficultyAbility.Schema
//    let sizes = [("Students",15);
//                 ("Questions",10);
//                 ("Answers",2)
//                 ("Responses",150)] |> Map.ofList   
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dStudentIDs   = sizes.["Students"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dQuestionIDs = sizes.["Questions"] |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dAnswers   = new Distributions.Bernoulli(0.5)
//
//    let x =  (((ONE,
//                [| for i in 1..sizes.["Students"]  -> ONE |]),
//               // [| for i in 1..sizes.["Answers"]   -> (ONE,"text") |]),
//                [| for i in 1..sizes.["Questions"] -> (((ONE,"question text"), dAnswers.Sample())) |]),
//                [| for i in 1..sizes.["Responses"] -> ((ONE,dStudentIDs.Sample()),dQuestionIDs.Sample()) |])
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//
//
//// simplified with just two answers
//module NaryDifficultyAbility =
//  let Schema =
//      {Name = "DifficultyAbility";
//        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real               ; Markup=Latent(CGaussian)} ]}
//                  //  "Answers",   {Columns=["text"      , {Type=T_String           ; Markup=Input}; ]};
//                  "Questions", {Columns=["text"      , {Type=T_String             ; Markup=Input};
//                                         "answer"    , {Type=T_Int                ; Markup=Input};
//                                         "difficulty", {Type=T_Real               ; Markup=Latent(CGaussian)};         
//                                        ]};
//                  "Responses", {Columns=["studentID" , {Type=T_Link "Students"  ; Markup=Input};
//                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
//                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty")),RealConst 1.0))) };
//                                         "guess"     , {Type=T_Int             ; Markup=Latent(Gen(DiscreteUniform(Const(6))))};
//                                         "answer"    , {Type=T_Int              ; Markup=Observable(Gen(If(Column("know"), Deref("questionID",Column "answer"), 
//                                                                                                                            Column("guess"))
//                                                                                                                           )) };
//                                         "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer")))) }; ]} ]}
//
//[<Fact>]
//let selftestNaryDifficultyAbility () =    
//    let schema = NaryDifficultyAbility.Schema
//    let sizes = [("Students",15);
//                 ("Questions",10);
//                 ("Answers",2)
//                 ("Responses",150)] |> Map.ofList   
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dStudentIDs   = sizes.["Students"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dQuestionIDs = sizes.["Questions"] |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dAnswers   = new Distributions.Discrete[| for i in 1..6 -> 1.0/(float) 6 |]
//
//    let x =  (((ONE,
//                [| for i in 1..sizes.["Students"]  -> ONE |]),
//               // [| for i in 1..sizes.["Answers"]   -> (ONE,"text") |]),
//                [| for i in 1..sizes.["Questions"] -> (((ONE,"question text"), dAnswers.Sample())) |]),
//                [| for i in 1..sizes.["Responses"] -> ((ONE,dStudentIDs.Sample()),dQuestionIDs.Sample()) |])
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//module ArrTest =
//  let Schema =
//      {Name = "ArrTest";
//        Tables = [
//                 
//                  "Questions", {Columns=[
//                                         
//                                          "answers",    {Type=T_Array(T_Link "Answers",Const 2)   ; Markup=Latent(Gen(Arr [| Const 666; Const 777;|]))} ]}; //broken
//                                         // "answers",    {Type=T_Array(T_Link "Answers")   ; Markup=Latent(Gen(Arr [| DiscreteUniform(Const 2); DiscreteUniform(Const 2);|]))} ]}; 
//                  ]}
//
//
//[<Fact>]
//let selftestArrTest () =    
//    let schema = ArrTest.Schema
//    let sizes = sizeMap schema.Tables
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dQuestions = sizes.["Questions"] |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    
//
//    let x = (ONE,    
//                
//             [| for i in 1..sizes.["Questions"] -> ONE |])
//               
//                
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//
//[<Fact>]
//let selftestDifficultyAbilityWrong () =    
//    let schema = DifficultyAbility.Schema
//    let sizes = sizeMap schema.Tables
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dStudent   = sizes.["Students"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dQuestions = sizes.["Questions"] |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dAnswers   = sizes.["Answers"]   |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//
//    let x = ((((ONE,
//                [| for i in 1..sizes.["Students"]  -> ONE |]),
//                [| for i in 1..sizes.["Answers"]   -> (ONE,"text") |]),
//                [| for i in 1..sizes.["Questions"] -> ((((((( (ONE,"question text"), dAnswers.Sample()),
//                                                                                    dAnswers.Sample()), 
//                                                                                    dAnswers.Sample()), 
//                                                                                    dAnswers.Sample()),
//                                                                                    dAnswers.Sample()), 
//                                                                                    dAnswers.Sample()), 
//                                                                                    dAnswers.Sample()) |]),
//                [| for i in 1..sizes.["Responses"] -> ((ONE,dStudent.Sample()),dQuestions.Sample()) |])
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//[<Fact>]
//let selftestDifficultyAbility () =    
//    let schema = DifficultyAbility.Schema
//    let sizes = [("Students",15);
//                 ("Questions",10);
//                 ("Answers",6)
//                 ("Responses",15*10)
//                // ("Responses",10)
//                ]|> Map.ofList
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dStudent   = sizes.["Students"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dQuestions = sizes.["Questions"] |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dAnswers   = sizes.["Answers"]   |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//
//    let x = ((((ONE,
//                [| for i in 1..sizes.["Students"]  -> ONE |]),
//                [| for i in 1..sizes.["Answers"]   -> (ONE,"text") |]),
//                [| for i in 1..sizes.["Questions"] -> ((((((( (ONE,"question text"), dAnswers.Sample()),
//                                                                                    0), 
//                                                                                    1), 
//                                                                                    2),
//                                                                                    3), 
//                                                                                    4), 
//                                                                                    5) |]),
//                 [| for s in 1..sizes.["Students"] -> [| for q in 1..sizes.["Questions"] -> ((ONE,s-1),q-1) |] |]|> FArray.concat
//             )
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//
//
//
//module DifficultyAbilityBuggyIf =
//  let Schema =
//      {Name = "DifficultyAbility";
//        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                      ; Markup=Latent(CGaussian)} ]}
//                  "Answers",   {Columns=["text"      , {Type=T_String           ; Markup=Input}; ]};
//                  "Questions", {Columns=["text"      , {Type=T_String                    ; Markup=Input};
//                                         "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer2"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer3"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer4"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "answer5"   , {Type=T_Link "Answers"            ; Markup=Input};
//                                         "difficulty", {Type=T_Real                       ; Markup=Latent(CGaussian)};         
//                                         "answers",    {Type=T_Array(T_Link "Answers",Const 6)   ; Markup=Observable(Gen(Arr [| Column "answer0" ; Column "answer1"; Column "answer2"; Column "answer3"; Column "answer4"; Column "answer5" |]))} 
//
//                                        ]};
//                  "Responses", {Columns=["studentID" , {Type=T_Link "Students"  ; Markup=Input};
//                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
//                                        
//                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty")),RealConst 1.0))) };
//                                           "guess"     , {Type=T_Int              ; Markup=Latent(Gen(DiscreteUniform(Const 6)))};
//                                         
//                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(If(Column("know"), Deref("questionID",Column "answer"),
//                                                                                                                            Index(Deref("questionID",Column "answers"), Column("guess")))
//                                                                                                         )) }; 
//                                          
//                                         
//                                          "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer"))))  }; 
//                                         ]} ]}
//
//[<Fact>]
//let selftestDifficultyAbilityBuggyIf () =    
//    let schema = DifficultyAbilityBuggyIf.Schema
//    let sizes = [("Students",15);
//                 ("Questions",10);
//                 ("Answers",6)
//                 ("Responses",15*10)
//                // ("Responses",10)
//                ]|> Map.ofList
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dStudent   = sizes.["Students"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dQuestions = sizes.["Questions"] |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let dAnswers   = sizes.["Answers"]   |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//
//    let x = ((((ONE,
//                [| for i in 1..sizes.["Students"]  -> ONE |]),
//                [| for i in 1..sizes.["Answers"]   -> (ONE,"text") |]),
//                [| for i in 1..sizes.["Questions"] -> ((((((( (ONE,"question text"), dAnswers.Sample()),
//                                                                                    0), 
//                                                                                    1), 
//                                                                                    2),
//                                                                                    3), 
//                                                                                    4), 
//                                                                                    5) |]),
//                 [| for s in 1..sizes.["Students"] -> [| for q in 1..sizes.["Questions"] -> ((ONE,s-1),q-1) |] |]|> Array.concat
//             )
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//module TestModelIf  =
//  let Schema =
//      {Name = "";
//        Tables = ["Table1", {Columns=[ "know"      , {Type=T_Bool             ; Markup=Input  };
//                                       "answer"    , {Type=T_Int              ; Markup=Latent(Gen(If(Column("know")  , Const(1), Const(0)))) }; ]} ]}
//
//[<Fact>]
//let selftestModelIf () =    
//    let schema = TestModelIf.Schema
//    let sizes = sizeMap schema.Tables
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dKnow   = sizes.["Table1"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//
//    let x = (ONE,
//                 [| for i in 1..sizes.["Table1"] -> (ONE, dKnow.Sample() = 1) |])
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//module TestModelEQ  =
//  let Schema =
//      {Name = "";
//        Tables = ["Table1", {Columns=[ "know"      , {Type=T_Bool             ; Markup=Input  };
//                                       "answer"    , {Type=T_Int              ; Markup=Latent(Gen(EQ(Column("know") , BoolConst(true)))) }; ]} ]}
//[<Fact>]
//let selftestModelEQ () =    
//    let schema = TestModelEQ.Schema
//    let sizes = sizeMap schema.Tables
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dKnow   = sizes.["Table1"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let x = (ONE,
//                 [| for i in 1..sizes.["Table1"] -> (ONE, dKnow.Sample() = 1) |])
//    let b = model.Test(x)
//    Assert.True(b)
//
//module TestGen  =
//  let Schema =
//      { Name = "";
//        Tables = ["Table1", {Columns=[ "know"      , {Type=T_Bool             ; Markup=Input  };
//                                       "answer"    , {Type=T_Int              ; Markup=Latent(Gen(Const(0))) }; ]} ]}
//[<Fact>]
//let selftestModelGen () =    
//    let schema = TestGen.Schema
//    let sizes = sizeMap schema.Tables
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let dKnow   = sizes.["Table1"]  |> (fun n -> new Distributions.Discrete([| for i in 1..n -> 1.0/(float) n |]))
//    let x = (ONE,
//                 [| for i in 1..sizes.["Table1"] -> (ONE, dKnow.Sample() = 1) |])
//    let b = model.Test(x)
//    Assert.True(b)
//
//
//[<Fact>]
//let testTrueSkill numPlayers numMatches =   
//    printfn "---------------------------------------------"
//    printfn "testTrueSkill %A %A" numPlayers numMatches 
//    let schema = TrueSkill.Schema
//    let sizes = Map.empty.Add("Players",numPlayers).Add("Matches", numMatches)
//    
//    let model = trDB schema sizes
//    let rnd = new System.Random()
//    //   printfn "%s" (model.ToString())
//    let x = (((ONE,
//                [| for i in 1..sizes.["Players"] -> ONE |]),
//              //  [| for i in 1..sizes.["Matches"] -> ((ONE,dP.Sample()),dP.Sample()) |])
//                [| for i in 1..sizes.["Matches"] -> ((ONE,rnd.Next(0,numPlayers-1)),rnd.Next(0,numPlayers-1)) |])
//            )
//
//    let b = model.Test(x)  
//    Assert.True(b)
//
//
//[<Fact>]
//let selftestTrueSkill () =    
//    let numPlayers = 100
//    let numMatches = 20000
//    let schema = TrueSkill.Schema
//    let sizes = Map.empty.Add("Players",numPlayers).Add("Matches", numMatches)
//    
//    let model = trDB schema sizes
//    let rnd = new System.Random()
//    //   printfn "%s" (model.ToString())
//    let x = (((ONE,
//                [| for i in 1..sizes.["Players"] -> ONE |]),
//              //  [| for i in 1..sizes.["Matches"] -> ((ONE,dP.Sample()),dP.Sample()) |])
//                [| for i in 1..sizes.["Matches"] -> ((ONE,rnd.Next(0,numPlayers-1)),rnd.Next(0,numPlayers-1)) |])
//            )
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//[<Fact>]
//let selftestTrueSkillLarge () =    
//    let numPlayers = 10000
//    let numMatches = 3500000
//    let schema = TrueSkill.Schema
//    let sizes = Map.empty.Add("Players",numPlayers).Add("Matches", numMatches)
//    
//    let model = trDB schema sizes
//    let rnd = new System.Random()
//    printfn "%s" (model.ToString())
//    let x = (((ONE,
//                [| for i in 1..sizes.["Players"] -> ONE |]),
//                [| for i in 1..sizes.["Matches"] -> ((ONE,rnd.Next(0,numPlayers-1)),rnd.Next(0,numPlayers-1)) |])
//            )
//
//    let b = model.Test(x)
//    Assert.True(b)
//[<Fact>]
//let selftestTrueSkillQuery () =    
//    let schema = TrueSkillQuery.Schema
//    let players = 4
//    let matches = players * players
//    let sizes = Map.empty.Add("Players",players).Add("Matches",matches).Add("HypotheticalMatches",matches)
//    //let sizes = sizeMap tables
//    let model = trDB schema sizes
//    printfn "%s" (model.ToString())
//    let x = (((ONE,
//                [| for i in 1..sizes.["Players"] -> ONE |]),
//                [| for i in 1..sizes.["Players"] -> [| for j in 1..sizes.["Players"] -> ((ONE,i-1),j-1) |] |] |> Array.concat),
//                [| for i in 1..sizes.["Players"] -> [| for j in 1..sizes.["Players"] -> ((ONE,i-1),j-1) |] |] |> Array.concat
//            )
//
//    let b = model.Test(x)
//    Assert.True(b)
//
//
//[<Fact>]
//let testDirichlet () =    
//    let m = Compiler.Discrete 5
//    let s = Sampler.FromModel(m)
//    printfn "w=%A" (s.Parameters)
//    let ys = [| for i in 1 .. 100 -> s.Sample() |]
//    printfn "ys=%A" ys
//
//    let l:LatentLearner.ILatentLearner<IDistribution<Vector>,unit,int,unit,IDistribution<int>,IDistribution<unit>> =
//         LatentLearner.LearnerFromModel
//              ({HyperParameter = (); Prior = <@ fun () -> (%m.Prior)()@>; Gen= <@ fun (w,x) -> ((%m.Gen)(w,()),()) @>})
//    let (dw,dz) = l.TrainPredict((),ys.[0])
//    printfn "dw=%A" dw
//    printfn "dz=%A" dz
//    Assert.True(true)
//
//
//[<Fact>]
//let testDirichletArray () =    
//    let m = IIDArray.M (Compiler.Discrete 5) 
//    let s = Sampler.FromModel(m)
//    printfn "w=%A" (s.Parameters)
//    let xs =  [| for i in 0 .. 100 -> () |]
//    let ys = s.Sample xs
//    printfn "ys=%A" ys
//
//    let l:LatentLearner.ILatentLearner<IDistribution<Vector>,unit[],int[],unit,IDistribution<int>[],IDistribution<unit>> =
//         LatentLearner.LearnerFromModel
//              ({HyperParameter = (); Prior = <@ fun () -> (%m.Prior)()@>; Gen= <@ fun (w,x) -> ((%m.Gen)(w,x),()) @>})
//    let (dw,dz) = l.TrainPredict(xs,ys)
//    printfn "dw=%A" dw
//    printfn "dz=%A" dz
//    Assert.True(true)
//[<Fact>]
//let testDirichletArrayCrossTalk () =    
//    let m = IIDArray.M (Compiler.Discrete 5) 
//    let mz = IIDArray.M (Compiler.Discrete 5) 
//    let s = Sampler.FromModel(m)
//    printfn "w=%A" (s.Parameters)
//    let xs =  [| for i in 0 .. 100 -> () |]
//    let ys = s.Sample xs
//    printfn "ys=%A" ys
//
//    let l:LatentLearner.ILatentLearner<IDistribution<Vector>,unit[],int[],int[],IDistribution<int>[],IDistribution<int>[]> =
//         LatentLearner.LearnerFromModel
//              ({HyperParameter = (); Prior = <@ fun () -> (%m.Prior)(),(%mz.Prior)()@>; Gen= <@ fun ((w1,w2),x) -> let y = (%m.Gen)(w1,x) in
//                                                                                                                   (y,y) @>})
//    let (dw,dz) = l.TrainPredict(xs,ys)
//    printfn "dw=%A" dw
//    printfn "dz=%A" dz
//    Assert.True(true)
//
//[<Fact>]
//let testDirichletArrayCrossTalkReal () =    
//    let m = IIDArray.M (Compiler.Discrete 2) 
//    let mz = IIDArray.M (Compiler.Discrete 2) 
//    let s = Sampler.FromModel(m)
//    printfn "w=%A" (s.Parameters)
//    let xs =  [| for i in 0 .. 100 -> () |]
//    let ys = s.Sample xs
//    printfn "ys=%A" ys
//
//    let l:LatentLearner.ILatentLearner<IDistribution<Vector>,unit[],int[],int[],IDistribution<int>[],IDistribution<int>[]> =
//         LatentLearner.LearnerFromModel
//              ({HyperParameter = (); Prior = <@ fun () -> (%m.Prior)(),(%mz.Prior)()@>; Gen= <@ fun ((w1,w2),x) -> let ys = (%m.Gen)(w1,x) in
//                                                                                                                   let zs = (%mz.Gen)(w2,x) in
//                                                                                                                   [| for i in range(ys) ->
//                                                                                                                          if zs.[i] = 0 then
//                                                                                                                            0
//                                                                                                                          else
//                                                                                                                            ys.[i] |],
//                                                                                                                   [| for i in range(ys) ->
//                                                                                                                          if zs.[i] = 0 then
//                                                                                                                            zs.[i]
//                                                                                                                          else
//                                                                                                                            zs.[i]  |]   @> })
//    let (dw,dz) = l.TrainPredict(xs,  ys)
//    printfn "dw=%A" dw
//    printfn "dz=%A" dz
//    Assert.True(true)
//
//
//    (*
//
//[<Fact>]
//let testDirichletArray () =    
//    let m = IID2n {HyperParameter = (); Prior = <@ fun () -> (%m.Prior)()@>; Gen= <@ fun (w,x) -> ((%m.Gen)(w,x),()) @>} 100
//    let s = Sampler.FromModel(m)
//    printfn "w=%A" (s.Parameters)
//    let xs =  [| for i in 0 .. 100 -> () |]
//    let ys = s.Sample xs
//    printfn "ys=%A" ys
//
//    let l:LatentLearner.ILatentLearner<IDistribution<Vector>,unit[],int[],unit,IDistribution<int>[],IDistribution<unit>> =
//         LatentLearner.LearnerFromModel
//              ({HyperParameter = (); Prior = <@ fun () -> (%m.Prior)()@>; Gen= <@ fun (w,x) -> ((%m.Gen)(w,x),()) @>})
//    let (dw,dz) = l.TrainPredict(xs,ys)
//    printfn "dw=%A" dw
//    printfn "dz=%A" dz
//    Assert.True(true)
//    *)
//
//
//
//
//
