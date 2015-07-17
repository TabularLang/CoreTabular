module MicrosoftResearch.Infer.Tabular.TypecheckerTest


//open MicrosoftResearch.Infer.Tabular.DataLayer

open Types
open Syntax
//open Print

type TestResult = TP //true positive
                | TN //true negative
                | FP //false positive
                | FN //false negative

type counterType = int * int * int * int

(*
let rec getFirstError (g:Env) (log:Table.Log) : ColumnName * string =
    match g with
    | G_Empty -> ("","")
    | G_Var ((y, t), tail) ->
        printf ""
        match (log.Item y) with
        | Table.Err msg -> ("dancing pigs",msg)
        | _ -> getFirstError tail log
    | G_Table ((y, t), tail) -> getFirstError tail log
    | G_Model ((y, t), tail) ->
        match (log.Item y) with
        | Table.Err msg -> ("dancing pigs",msg)
        | _ -> getFirstError tail log

let getErrors (log:Table.Log) : string =
    let folder  = fun curr c value ->
                    match value with
                    Table.Err msg -> c + ":" + msg + "\n"
                    | _ -> curr
    Map.fold folder "" log
*)

let testOnSchema (g:Env) ((name, s, positive):(string * Syntax.Schema * bool)) : TestResult =
  //  let (log, err, _) = Schema.synthSchema g s
    let (log, err, _) = Schema.typeSchema s
    let errors = Map.fold (fun s tb log -> Map.fold (fun s col v -> 
                                                     match v with 
                                                     |  (Table.Err msg) ->
                                                      s+(sprintf "\n %A %A : %A" tb col msg)
                                                     | _ -> s) 
                                                     s log)

                          "" log                                   


    if not err then
      if positive then
         printf "Typechecking %O succeeded, as expected\n" name
         TP
      else
         printf "Error: typechecking %O should have failed\n found errors:%A" name errors
         FP
    else
        if positive then
           printf "Error: typechecking %O failed\n--Error message: %A  \n" name errors
           FN
        else
           printf "Typechecking %O failed as expected\n--Error message: %A \n" name errors
           TN

let updateCounter (c:counterType) (res:TestResult) : counterType =
    let (tp, tn, fp, fn) = c
    match res with
    | TP -> (tp + 1, tn, fp, fn)
    | TN -> (tp, tn + 1, fp, fn)
    | FP -> (tp, tn, fp + 1, fn)
    | FN -> (tp, tn, fp, fn + 1)

[<EntryPoint>]
let main args =
(*
    //let e = Const(13)
    let e = Bernoulli(Gaussian(RealConst(0.2), Gamma(RealConst(0.3), RealConst(0.4))))
    let t = Checker.synthExpr (Types.G_Empty) e

    let m1:Model = Gen (Gaussian (RealConst (0.0), RealConst (0.0001)))
    let m2:Model = (Hyper (RealConst (1.0)))
    let m3:Model = Gen (Gaussian (Column ("Mean"), Column ("Prec"))) //???

    let c1 : Column<Markup> = {Type = T_Real; Markup = Latent (m1)}
    let c2 : Column<Markup> = {Type = T_Real; Markup = Latent (m2)}
    let c3 : Column<Markup> = {Type = T_Real; Markup = Observable (m3)}

    let T1 : Table<Markup> = {Columns = [("Mean", c1); ("Prec", c2); ("Item", c3)] }

    let S : Database<Markup> = {Name = "TestDatabase"; Tables = [("Gaussians", T1)]}

    let (log, err ,t1 : Types.DatabaseT) = Schema.synthSchema (Types.G_Empty) S

    //printf "Synthed type: %s\n" (Tabular.columnTypeToTex t)
    //printf "%s\n" (dataBaseToTex 

    //...
    let Schema1 =
      {Name = "CoinFlips";
      Tables = [ "CoinFlips",  {Columns=["hAlpha",      {Type= T_Real; Markup=Latent(Hyper(RealConst(1.0)))};
                                     "hBeta",      {Type=T_Real; Markup=Latent(Hyper(RealConst(1.0)))};
                                     //This should be Beta, not Gaussian
                                     "Bias",        {Type=T_Real; Markup=Latent(Static(Gen(Gaussian(Column "hAlpha", Column "hBeta"))))};
                                     "B",  {Type=T_Bool; Markup=Observable(Gen(Bernoulli(Column "Bias")))} ]} ]}

    //let dm = DAREEval.DAREUI
    let ts = TrueSkill.Schema // etc.
    //let ts = PureRecommender.Schema
    //let ts = TrueSkillObservable.Schema
    //let ts = Schema1
    //let ts = LDAExample.Schema
    *)
    let counter:counterType = (0, 0, 0, 0)

    //let ts0 = Schema.synthSchema g0 InfernoClassicMM.Schema
    //let ts1 = Schema.synthSchema g0 DifficultyAbilitySkip.Schema
    //let out1 = testOnSchema DifficultyAbilitySkip.Schema true
    //printf "typechecking succeeded \n"
    
    let newSchemas =
      
      [("TrueSkill",
        [Declaration(Table("Players",  None),
         ["Skill",   {Type=T_Real; Markup=Latent(MExp(Dist(GaussianFromMeanAndPrecision,[Const (RealConst 25.); Const(RealConst 10.)])))}]);
         Declaration(Table("Matches",  None),
                       ["Player1", {Type=T_Link "Players"; Markup=Input};
                        "Player2", {Type=T_Link "Players"; Markup=Input};
                        "Perf1",   {Type=T_Real; Markup=Latent(MExp(Dist(GaussianFromMeanAndPrecision,[DeRef(Var "Player1", "Players", "Skill"); Const(RealConst 1.0)])))};
                        "Perf2",   {Type=T_Real; Markup=Latent(MExp(Dist(GaussianFromMeanAndPrecision,[DeRef(Var "Player2", "Players", "Skill"); Const(RealConst 1.0)])))};
                        "Win1" ,   {Type=T_Bool; Markup=Observable(MExp(Prim(Gt, [Var "Perf1"; Var "Perf2"])))} ]) ],
        true)
      ]
      @
      List.map (fun (n,s) -> (n,s,true)) NewModels.DAREProgression 



//
//
//    let legacySchemas = 
//            
//             List.foldBack 
//                 (fun  (schema,b) rest -> 
//                       try ("(legacy)"+schema.Name,OldToNew.trSchema schema,b) :: rest
//                       with e -> 
//                           printfn  "skipping untranslateable schema %A " schema.Name
//                           match e with 
//                               | Failure s -> printfn  "failure: %A " s
//                               | e ->  printfn  "failure: %A " e
//                           rest
//                       ) 
//    
//                      [(InfernoClassicMM.Schema, true); 
//                       (TrueSkillObservable.Schema, true);
//                       (TrueSkill.Schema, true); 
//                       (TrueSkillQuery.Schema, true);
//                       (LDAExample.Schema, true); 
//                       (Recommender.Schema, true);
//                       (PureRecommender.Schema, true); 
//                       (PureRecommender.SchemaWithUpto, true);
//                       (RecommenderQuery.Schema, true);
//                        (ConjugateGaussian.Schema, true);
//                       (GenGaussian.Schema, true); 
//                       (ConjugateGaussianWithLatent.Schema, true);
//                       (GaussianWithPrior.Schema, true); 
//                       (DifficultyAbility.Schema, false);
//                       (DifficultyAbilityCG.Schema, false); 
//                       (DifficultyAbilityEvaluation.Schema, false);
//                       (DifficultyAbilityWCorrect.Schema, false);
//                       (DifficultyAbilityDiscrete.Schema, true);
//                       (DifficultyAbilityUntrainedDiscreteEvaluation.Schema, true);
//                       (DifficultyAbilityDiscreteEvaluation.Schema, true);
//                       (DifficultyAbilitySkipDiscreteEvaluation.Schema, true);
//                       (DifficultyAbilitySkipGuessDiscreteEvaluation.Schema, true);
//                       (DifficultyAbilitySkipGuessCheatDiscreteEvaluation.Schema, true);
//                       (DifficultyAbility.Schema, false);
//                       (DifficultyAbilitySkip.Schema, false);
//                       (DifficultyAbilitySkipCheat.Schema, false);
//                       (DARE.Participant, true); (DARE.Question, true); (DARE.DARE, true);
//                       (DAREQuery.Participant, true); (DAREQuery.Question, true);
//                       (DAREQuery.DARE, true)]
//                 []
//    
//    let testSchemas = 
//        newSchemas @ legacySchemas

//  //  let (logStrLib,errLib,gInit) = Library.buildLibrary ()
//    let (logStrLib,errLib,gInit) = (Map.empty,false,G_Empty)
//    if errLib then
//      printf "Error in typechecking library:\n%O" logStrLib
//      ignore( System.Console.ReadLine())
//      1
//    else
//      let folder = fun counter -> fun s -> updateCounter counter (testOnSchema gInit s )
//      let counter = List.fold folder (0, 0, 0, 0) testSchemas
//      let (tp, tn, fp, fn) = counter
//      printf "final state: TP:%d, TN:%d, FP:%d, FN:%d\n" tp tn fp fn
//      ignore( System.Console.ReadLine())
//      0
    0
