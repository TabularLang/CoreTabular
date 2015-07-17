
// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

//namespace MicrosoftResearch.Infer.Fun.FunDBLayer

//TODO:
// add generic pre-pass to collect required ranges - what we do now will break.
// complete missing expressions (arrays lets), factors and distributions
// remove vestigial support for hypers
// split into syntax, translate, interp modules
// delete CSoft.fs, Program.fs
// move tests and specific ExcelCompiler elsewhere to remove dependencies on DataLayer and MarkupFunlayer...  we should just depend on Infer.NET and pure Tabular
// split typechecking from compilation - that's easy to do now.


[<AutoOpen>]
module CompilerTest
  open MicrosoftResearch.Infer.Tabular
  open Compiler
  open Syntax
  module Tabular = Syntax
  open MicrosoftResearch.Infer
  open MicrosoftResearch.Infer.Factors

 

  module T = Syntax


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


      
  let array (a:'T[]) : System.Array = a :> System.Array

  let mkData numPlayers numMatches =
      let dSkills =  new Distributions.Gaussian(0.0,1.0)
      let v = [| for i in 0 .. numPlayers - 1 -> 1.0/(float) numPlayers|]
      let dPlayers = if numPlayers > 0 then new Distributions.Discrete(v) else null
      let skills = [| for i in 0 .. numPlayers - 1 -> dSkills.Sample() |]
      let player1 = [| for i in 0 .. numMatches-1 -> dPlayers.Sample() |]
      let player2 = [| for i in 0 .. numMatches-1 -> dPlayers.Sample() |]
      let player1Wins =  [| for i in 0 .. numMatches-1 -> (new Distributions.Gaussian(skills.[player1.[i]],1.0)).Sample() >  (new Distributions.Gaussian(skills.[player2.[i]],1.0)).Sample() |]
      let data = 
        Map.ofList
          [("Players", (numPlayers,Map.empty));
           ("Matches", (numMatches,(Map.ofList([("Player1",array player1 );
                                                ("Player2",array player2 );
                                                ("Player1Wins",array player1Wins)
                                      ]))))]
      data



  let testTrueSkillWithConjugate numPlayers numMatches =
     
      let schema =
       [Declaration(Table("Players", None),  
              ["Precision",{Type=T_Real; Markup=Hyper(T.Const(T.RealConst 10.))};
              // "Mean",
              //       {Type=T_Real; Markup=Param(MCall("CGaussian",[]))};
               "Skill",
                     {Type=T_Real; Markup=Latent(MCall("CGaussian",[]))}]);
        Declaration(Table("Matches",  None),
                       ["Player1", {Type=T_Link "Players"; Markup=Input};
                        "Player2", {Type=T_Link "Players"; Markup=Input};
                        "Perf1",   {Type=T_Real; Markup=Latent(MExp(T.Dist(GaussianFromMeanAndPrecision,[DeRef(T.Var "Player1", "Players", "Skill"); T.Const(T.RealConst 1.0)])))};
                        "Perf2",   {Type=T_Real; Markup=Latent(MExp(T.Dist(GaussianFromMeanAndPrecision,[DeRef(T.Var "Player2", "Players", "Skill"); T.Const(T.RealConst 1.0)])))};
                        "Player1Wins" ,   {Type=T_Bool; Markup=Observable(MExp(T.Prim(Gt, [T.Var "Perf1"; T.Var "Perf2"])))} ]) ]

      //compile once
      let infer = time "compile" compile (schema)
      
      let data1 = mkData numPlayers numMatches
      //time "infer" infer data1

      let (e,(VD,AD)) = time "infer" infer data1
      printfn "Log Evidence: %O" e.LogOdds
      Map.iter (fun k v -> printfn "Variable %O\n-------------------------------\n%O" k v) VD
      Map.iter (fun k v -> printfn "Array %O\n-------------------------------\n%O" k v) AD

  let testTrueSkillWithHyperAndParam numPlayers numMatches =
     
      let schema =
       [Declaration(Table("Players",  None),
              ["Precision",{Type=T_Real; Markup=Hyper(T.Const(T.RealConst 10.))};
               "Mean",
                     {Type=T_Real; Markup=Param(MCall("CGaussian",[]))};
               "Skill",
                     {Type=T_Real; Markup=Latent(MExp(T.Dist(GaussianFromMeanAndPrecision,[T.Var "Mean"; T.Var "Precision"])))}]);
        Declaration(Table("Matches",  None),
                       ["Player1", {Type=T_Link "Players"; Markup=Input};
                        "Player2", {Type=T_Link "Players"; Markup=Input};
                        "Perf1",   {Type=T_Real; Markup=Latent(MExp(T.Dist(GaussianFromMeanAndPrecision,[DeRef(T.Var "Player1", "Players", "Skill"); T.Const(T.RealConst 1.0)])))};
                        "Perf2",   {Type=T_Real; Markup=Latent(MExp(T.Dist(GaussianFromMeanAndPrecision,[DeRef(T.Var "Player2", "Players", "Skill"); T.Const(T.RealConst 1.0)])))};
                        "Player1Wins" ,   {Type=T_Bool; Markup=Observable(MExp(T.Prim(Gt, [T.Var "Perf1"; T.Var "Perf2"])))} ]) ]

      //compile once
      let infer = time "compile" compile (schema)
      
      let data1 = mkData numPlayers numMatches
      //time "infer" infer data1

      let (e,(VD,AD)) = time "infer" infer data1
      printfn "Log Evidence: %O" e.LogOdds
      Map.iter (fun k v -> printfn "Variable %O\n-------------------------------\n%O" k v) VD
      Map.iter (fun k v -> printfn "Array %O\n-------------------------------\n%O" k v) AD

  let testCSoftTrueSkill numPlayers numMatches =
      let db = TrueSkill.Schema

      //compile once
      let infer = time "compile" compile (OldToNew.trSchema(db))
      
      let data1 = mkData numPlayers numMatches
      time "infer" infer data1


      
  

  let testCSoftTrueSkillMultiple numPlayers numMatches =
      let db = TrueSkill.Schema

      //compile once
      let infer = compile (OldToNew.trSchema(db))

       // infer several times with different data and no-recompile
      let data1 = mkData 0 0
      ignore(infer data1)
      
     
      let data1 = mkData numPlayers numMatches
      ignore(infer data1)

      let data2 = mkData (numPlayers*2) (numMatches*2)
      ignore(infer data2)
  
  
  let testCSoftTrueSkillScales numPlayers numMatches =
      let db = TrueSkill.Schema
      let infer = compile (OldToNew.trSchema(db))


      let data1 = mkData 10 10
      ignore(infer data1) // JIT once
      
      
      let s = new System.Diagnostics.Stopwatch();
      let cp = System.Diagnostics.Process.GetCurrentProcess();
      printfn "Players,Matches,Time_ms,PeakWorkingSet_MB"
      let rec scale numMatches = 
        printf "%A,%A," numPlayers numMatches 
        let data1 = mkData numPlayers numMatches
        s.Reset();
        s.Start()
        ignore(infer data1)
        s.Stop()
        cp.Refresh()
        let pM = cp.PeakWorkingSet64
        printfn "%A,%A" (System.Convert.ToInt32(s.ElapsedMilliseconds)) (System.Convert.ToInt32(pM/ (1024L*1024L)))
        scale (numMatches * 10) 
      scale (numMatches)

//  open OldService
//  let testDAREUI()  =
//  
//
//    let db = (DAREEval.DAREUI)
//    printfn "%O" (Tabular.dataBaseToTex db)
//    
//    let loader = new DAOLoader(__SOURCE_DIRECTORY__ + "\..\MarkupFunLayer\DARE.accdb")
//   
//    let concreteData, posToID = readTable loader db
//    
//
//    let size t = 
//       let m,os = concreteData.[t]
//       Seq.length(os)
//
//    let column (dummy:'T) t c = 
//           let m,os = concreteData.[t]
//           (c,([| for o in os -> o.[m.[c]] :?> 'T |] :> System.Array))
//
//    let data = 
//        Map.ofList
//          [("Participants",(size "Participants",Map.empty));
//           ("Questions", (size "Questions",Map.empty));
//           ("QuestionsTrain", (size "QuestionsTrain",Map.ofList([column 0 "QuestionsTrain" "QuestionID";
//                                                                 column 0 "QuestionsTrain" "Answer"])))
//           ("Responses", (size "Responses",Map.ofList([column 0 "Responses" "QuestionID";
//                                                         column 0 "Responses" "ParticipantID"])));
//           ("ResponsesTrain", (size "ResponsesTrain",Map.ofList([column 0 "ResponsesTrain" "ResponseID";
//                                                                 column 0 "ResponsesTrain" "Answer"])))];
//              
//    let infer = compile  (OldToNew.trSchema db)
//
//    
//    infer data
//      
//     
//  
//  let testRecommender() = 
//      
//      let db = PureRecommender.SchemaWithUpto
//   (* this version adds a latent CGaussian index model to check whether compound parameters are compiled correctly for indexed models
//      let db = 
//       let Const = T.Const
//       {Name = "MovieRatingsWithUpto";
//        Tables = [ "Users",   {Columns=["UserCluster",   {Type=T_Upto(Const 4);          Markup=Latent(CDiscreteWith(Const 4))};
//                                        "IsMale",        {Type=T_Bool;                   Markup=Observable(Array(CBernoulli,[Column "UserCluster"]))};
//                                        "Age",           {Type=T_Upto(Const 100);        Markup=Observable(Array(CDiscreteWith(Const 100),[Column "UserCluster"]))} ]}
//                   "Movies",  {Columns=["MovieCluster",  {Type=T_Upto(Const 4);          Markup=Latent(CDiscreteWith(Const 4))};
//                                        "Category",      {Type=T_Upto(Const 20);         Markup=Observable(Array(CDiscreteWith(Const 20),[Column "MovieCluster"])) };
//                                        "Year",          {Type=T_Upto(Const 100);        Markup=Observable(Array(CDiscreteWith(Const 100),[Column "MovieCluster"])) } ]};
//                   "Ratings", {Columns=["UserID",        {Type=T_Link "Users";           Markup=Input};
//                                        "MovieID",       {Type=T_Link "Movies";          Markup=Input};
//                                        "Rating",        {Type=T_Upto(Const 6);          Markup=Observable(Array(CDiscreteWith(Const 5), [Deref("UserID", Column "UserCluster"); 
//                                                                                                                                          Deref("MovieID", Column "MovieCluster")]))};
//                                        "Score",         {Type=T_Real;                   Markup=Latent(Array(CGaussian, [Deref("UserID", Column "UserCluster"); 
//                                                                                                                            Deref("MovieID", Column "MovieCluster")]))}                                                                                        
//                                        ]}    
//       ]}
//    *)
//     // let s = trDatabase(db)
//      
//      
//   //   let (RE,VE,AE) = interpS Map.empty Map.empty Map.empty s
//      let sizeUserClusters = 4
//      let sizeMovieClusters = 4
//      let users = 10
//      let movies = 200
//      let ratings = users * 50
//      let dusers = new Distributions.Discrete([| for i in 0..users-1 -> 1.0/(float) users |])
//      let dmovies = new Distributions.Discrete([| for i in 0..movies-1 -> 1.0/(float) movies |])
//      let duserCluster = new Distributions.Discrete([| for i in 0..sizeUserClusters-1 -> 1.0/(float) sizeUserClusters |])
//      let dmovieCluster = new Distributions.Discrete([| for i in 0..sizeMovieClusters-1 -> 1.0/(float) sizeMovieClusters |])
//      let userClusters = [| for i in 0..users-1 -> duserCluster.Sample()|]
//      let movieClusters = [| for i in 0..movies-1 -> dmovieCluster.Sample()|]
//      let disMale = [| for i in 1 .. sizeUserClusters-> new Distributions.Bernoulli((float) i * 0.25) |]
//      let dAge = [| for i in 1 .. sizeUserClusters -> Distributions.Discrete.UniformInRange(100,(i-1)*25,i*25-1) |] 
//      let dCategory =  [| for i in 1 .. sizeMovieClusters -> Distributions.Discrete.UniformInRange(20,(i-1) * (20/sizeMovieClusters),i * (20/sizeMovieClusters)-1) |]
//      let dYear =  [| for i in 1 .. sizeMovieClusters -> Distributions.Discrete.UniformInRange(100,(i-1) * (100/sizeMovieClusters),i * (100/sizeMovieClusters)-1) |]
//      let dRatingss = [| for i in 0 .. sizeUserClusters-1 -> [| for j in 0 .. sizeMovieClusters-1 ->
//                                                                let diff = System.Math.Abs(i-j)
//                                                                Distributions.Discrete.UniformInRange(5,diff,diff+1) |] |]
//       //compile once
//      let infer = time "compile" compile  (OldToNew.trSchema(db))
//      
//      let data = 
//        Map.ofList
//          [("Users",(users,Map.ofList([("IsMale",[|for i in 0..users-1-> disMale.[userClusters.[i]].Sample()  |]:> System.Array)
//                                       ("Age",[|for i in 0..users-1-> dAge.[userClusters.[i]].Sample()  |]:> System.Array)
//                                       ("TrueUserCluster",userClusters:>System.Array)
//                                      ])));
//           ("Movies", (movies,Map.ofList([("Category",[|for i in 0..movies-1-> dCategory.[movieClusters.[i]].Sample()  |]:> System.Array)
//                                          ("Year",[|for i in 0..movies-1-> dYear.[movieClusters.[i]].Sample()  |]:> System.Array)
//                                          ("TrueMovieCluster",movieClusters:>System.Array)
//                                      ])));
//           (let userIDs =  [|for i in 1..ratings-> dusers.Sample()  |]
//            let movieIDs = [|for i in 1..ratings-> dmovies.Sample()  |]
//            ("Ratings", (users*50,Map.ofList([("UserID",userIDs:> System.Array);
//                                             ("MovieID",movieIDs:> System.Array);
//                                             ("Rating",[|for i in 0..ratings-1-> (dRatingss.[userClusters.[userIDs.[i]]].[movieClusters.[movieIDs.[i]]]).Sample() |]:> System.Array)
//                                            ]))))
//          ]
//
//
//      do
//        Map.iter(fun tbl (size,colMap) ->
//                 printfn "%A" tbl
//                 printfn "------------------"
//                 printf "ID,"
//                 Map.iter(fun cn _ -> printf "%O," cn) colMap
//                 printfn ""
//                 for i in 0..size-1 do
//                    printf "%O," i
//                    Map.iter(fun cn (cv:System.Array) -> printf "%O," (cv.GetValue(i))) colMap
//                    printfn ""
//                 printfn "------------------"
//                 ) data
//                        
//        
//     
//      let (e,(VD,AD)) = time "infer" infer data
//      printfn "Log Evidence: %O" e.LogOdds
//      Map.iter (fun k v -> printfn "Variable %O\n-------------------------------\n%O" k v) VD
//      Map.iter (fun k v -> printfn "Array %O\n-------------------------------\n%O" k v) AD
      


//  
//
//  let test() = 
//      //let db = TrueSkill.Schema
//     // let db = DifficultyAbilitySkipDiscreteEvaluation.Schema
//     //let db = DAREEval.DAREUI
//      //let db = InfernoClassicMM.Schema
//      let db =PureRecommender.SchemaWithUpto
//      let s = trSchema( OldToNew.trSchema(db))
//      
//      let cs = StoString "\n" s
//      printfn "%s\n" cs
//      let (RE,VE,AE) = interpS Map.empty Map.empty Map.empty s
//      printfn "%A \n %A \n %A " RE VE AE
//
//  let dareEvaluation (schema:OldTabular.Database<_>) =  
//    printfn "evaluating %A" schema.Name
//
//    let db = schema
//   // printfn "%O" (Tabular.dataBaseToTex db)
//    
//    let loader = new DAOLoader(__SOURCE_DIRECTORY__ + "\..\MarkupFunLayer\DARE.accdb")
//   
//    let concreteData, posToID = readTable loader db
//    
//
//    let size t = 
//       let m,os = concreteData.[t]
//       Seq.length(os)
//
//    let column (dummy:'T) t c = 
//           let m,os = concreteData.[t]
//           (c,([| for o in os -> o.[m.[c]] :?> 'T |] :> System.Array))
//
//    let data = 
//        Map.ofList
//          [("Participants",(size "Participants",Map.empty));
//           ("Questions", (size "Questions",          Map.ofList([
//                                                                 column 0 "Questions" "TrueAnswer";
//                                                                 column true "Questions" "Training";
//                                                                 ])))
//           ("QuestionsTrain", (size "QuestionsTrain",Map.ofList([column 0 "QuestionsTrain" "QuestionID";
//                                                                 column 0 "QuestionsTrain" "Answer";
//                                                                 ])))
//           ("Responses", (size "Responses",Map.ofList([column 0 "Responses" "QuestionID";
//                                                       column true "Responses" "Training";
//                                                       column 0 "Responses" "ParticipantID";
//                                                       column 0 "Responses" "TrueAnswer";])));
//           ("ResponsesTrain", (size "ResponsesTrain",Map.ofList([column 0 "ResponsesTrain" "ResponseID";
//                                                                 column 0 "ResponsesTrain" "Answer"])))];
//              
//    let infer = 
//      time "Tabular translation" compile  (OldToNew.trSchema db)
//
//    
//    let (e,(VD,AD)) = 
//      time "Tabular inference" infer data
//    
//    let report = fun  table ->
//    
//      let (actualcolMap, actuals) = concreteData.[table]
//      if true then 
//        let mutable logProbTest = 0.
//    
//        let mutable countTest=0
//        let mutable numCorrect=0;
//        let mutable idx = 0;
//        let predicted_answer = AD.[col(table,"Answer")] :?> DistributionArray<Discrete>
//        for actual in actuals do
//          if (not(actual.[actualcolMap.["Training"]] :?> bool)) 
//          then
//            countTest <- countTest+1
//            let answer = predicted_answer.[idx]
//            let trueAnswer =  actual.[actualcolMap.["TrueAnswer"]] :?> int
//            let mode = answer.GetMode()
//            if (mode = trueAnswer) then numCorrect <- numCorrect+1
//            logProbTest <- logProbTest + answer.GetLogProb(trueAnswer)
//          idx <- idx + 1
//        printfn "Test set size %A" countTest
//        printfn "Test set accuracy %.2f (percent)" (100.0* ((float) numCorrect/ (float) countTest))
//        printfn "Avg log prob TrueAnswer %.3f (test)" (logProbTest / (float)countTest)
//        printfn "Avg prob TrueAnswer %.3f (test)" (System.Math.Exp(logProbTest / (float)countTest))
//      else()
//    report "Responses"
//    report "Questions"
//    printfn "Model Evidence %A" e.LogOdds
    
//   
//   
//  let dareProgressionEvaluation () = 
//    dareEvaluation(DAREEval.Ability) 
//    printfn "\n---------------------"
//    dareEvaluation(DAREEval.DA) 
//    printfn "\n---------------------"
//    dareEvaluation(DAREEval.DARE) 
//
//  let moocEvaluation (schema:OldTabular.Database<_>) =  
//    printfn "evaluating %A" schema.Name
//
//    let db = schema
//   // printfn "%O" (Tabular.dataBaseToTex db)
//    
//    let loader = new DAOLoader(__SOURCE_DIRECTORY__ + "\..\MarkupFunLayer\mooc - evaluation.accdb")
//   
//    let concreteData, posToID = readTable loader db
//    
//
//    let size t = 
//       let m,os = concreteData.[t]
//       Seq.length(os)
//
//    let column (dummy:'T) t c = 
//           let m,os = concreteData.[t]
//           (c,([| for o in os -> o.[m.[c]] :?> 'T |] :> System.Array))
//
//    let data = 
//        Map.ofList
//          [("Students",(size "Students",Map.empty));
//           ("Questions", (size "Questions",          Map.ofList([
//                                                                 column 0 "Questions" "answerIdx";
//                                                                 ])))
//           ("Responses", (size "Responses",Map.ofList([column 0 "Responses" "studentID";
//                                                       column 0 "Responses" "questionID";
//                                                       column true "Responses" "Training";
//                                                       
//                                                       column 0 "Responses" "answerIdx";])));
//           ("ResponsesTrain", (size "ResponsesTrain",Map.ofList([column 0 "ResponsesTrain" "studentID";
//                                                                 column 0 "ResponsesTrain" "questionID";
//                                                                 column 0 "ResponsesTrain" "answerIdx"])))];
//              
//    let infer = 
//      time "Tabular translation" compile (OldToNew.trSchema(db))
//
//    
//    let (e,(VD,AD)) = 
//      time "Tabular inference" infer data
//    
//    let report = fun  table ->
//    
//      let (actualcolMap, actuals) = concreteData.[table]
//      if true then 
//        let mutable logLikelihoodTraining = 0.
//        let mutable logLikelihoodTest = 0.
//        let mutable countTraining=0
//        let mutable countTest=0
//        let mutable idx = 0;
//        let mutable numCorrectTraining = 0;
//        let mutable numCorrectTest = 0;
//        let predicted_answer = AD.[col(table,"answerIdxP")] :?> DistributionArray<Discrete>
//        for actual in actuals do
//          if (not(actual.[actualcolMap.["Training"]] :?> bool)) 
//          then
//            countTest <- countTest+1
//            let answer = predicted_answer.[idx]
//            let trueAnswer =  actual.[actualcolMap.["answerIdx"]] :?> int
//            let mode = answer.GetMode()
//            if (mode = trueAnswer) then numCorrectTest <- numCorrectTest+1
//            logLikelihoodTest <- logLikelihoodTest + answer.GetLogProb(trueAnswer)
//          else
//            countTraining <- countTraining+1
//            let answer = predicted_answer.[idx]
//            let trueAnswer =  actual.[actualcolMap.["answerIdx"]] :?> int
//            let mode = answer.GetMode()
//            if (mode = trueAnswer) then numCorrectTraining <- numCorrectTraining+1
//            logLikelihoodTraining <- logLikelihoodTraining + answer.GetLogProb(trueAnswer)
//          idx <- idx + 1
//
//        printfn "Training set size %A" countTraining
//        printfn "Training set accuracy %A (percent)" (100.0* ((float) numCorrectTraining/ (float) countTraining))
//        printfn "logLikelihood TrueAnswer %A (training)" (logLikelihoodTraining / (float) countTraining)
//
//        printfn "Test set size %A" countTest
//        printfn "Test set accuracy %A (percent)" (100.0* ((float) numCorrectTest/ (float) countTest))
//        printfn "logLikelihood TrueAnswer %A (test)" (logLikelihoodTest / (float)countTest)
//      else()
//    report "Responses"
//    printfn "Model Evidence %A" e.LogOdds
//    
//   
//   
//  let moocProgressionEvaluation () = 
//    moocEvaluation(MOOCEval.DA) 
//    printfn "\n---------------------"
//    moocEvaluation(MOOCEval.DAS) 
//    printfn "\n---------------------"
//    moocEvaluation(MOOCEval.DASG)
//    printfn "\n---------------------"
//    moocEvaluation(MOOCEval.DASGC)


