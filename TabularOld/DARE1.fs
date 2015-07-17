namespace MicrosoftResearch.Infer.Tabular



                  
module DAREEval =
  open Tabular
  let answerRange = 8
  let abilityPrior = Gaussian(RealConst 0., RealConst 1.)
  let dampingFactor = RealConst 0.2 
  let difficultyPrior = Gaussian(RealConst 0., RealConst 1.)
  let discriminationPrior = Gamma(RealConst 5.0, RealConst 0.2)
  let T_AnswerRange = T_Upto (Const answerRange)
  let Ability =
      {Name = "Ability";
        Tables = ["Participants",  
                               {Columns=["Ability"   , {Type=T_Real               ; Markup=Latent(Gen(abilityPrior))} ]};
                  "Questions", {Columns=["Answer"  ,   {Type=T_AnswerRange                ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))} ;
                                         "TrueAnswer", {Type=T_AnswerRange                ; Markup=Input};
                                         "Training"   , {Type=T_Bool              ; Markup=Input};
                                        ]};
               
                  "QuestionsTrain", {Columns=["QuestionID", {Type=T_Link "Questions"  ; Markup=Input};
                                              "Answer"  , {Type=T_AnswerRange                 ; Markup=Observable(Gen(Deref("QuestionID",Column "Answer")))};   
                                        ]};
               
                  "Responses", {Columns=
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input}; 
                                         "Training"   , {Type=T_Bool            ; Markup=Input}; 
                                         "Advantage",   {Type=T_Real            ; Markup=Latent(Gen(DampBackward(
                                                                                                        Deref("ParticipantID",Column "Ability"),
                                                                                                        dampingFactor)))};
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Column("Advantage"),
                                                                                                           RealConst 1.0))) };
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Latent(Gen(StrictIf(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                       Column("Guess")))) };
                                         "TrueAnswer", {Type=T_AnswerRange             ; Markup=Input};
                                        ]};                 
                  "ResponsesTrain", {Columns=
                                        ["ResponseID", {Type=T_Link "Responses" ; Markup=Input };
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(Deref("ResponseID",Column "Answer"))) };                                                                             
                                        ]}
                  ]}
 
  let DA =
      {Name = "DA";
        Tables = ["Participants",  
                               {Columns=["Ability"   , {Type=T_Real               ; Markup=Latent(Gen(abilityPrior))} ]};
                  "Questions", {Columns=["Answer"  ,   {Type=T_AnswerRange                ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))} ;
                                         "Difficulty", {Type=T_Real               ; Markup=Latent(Gen(difficultyPrior))}; //new 
                                         "TrueAnswer", {Type=T_AnswerRange                ; Markup=Input};
                                         "Training"   , {Type=T_Bool              ; Markup=Input};
                                        ]};
                  "QuestionsTrain", {Columns=["QuestionID", {Type=T_Link "Questions"  ; Markup=Input};
                                           "Answer"  , {Type=T_AnswerRange                 ; Markup=Observable(Gen(Deref("QuestionID",Column "Answer")))};   

                                        ]};
                  "Responses", {Columns=
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input}; 
                                         "Training"   , {Type=T_Bool            ; Markup=Input}; 
                                         "Advantage",   {Type=T_Real            ; Markup=Latent(Gen(DampBackward(
                                                                                                        Minus(Deref("ParticipantID",Column "Ability"),
                                                                                                                   Deref("QuestionID",Column "Difficulty")),
                                                                                                        dampingFactor)))};
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Column("Advantage"),
                                                                                                           RealConst 1.0))) };
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Latent(Gen(StrictIf(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                       Column("Guess")))) };
                                         "TrueAnswer", {Type=T_AnswerRange             ; Markup=Input};
                                        ]};                 
                  "ResponsesTrain", {Columns=
                                        ["ResponseID", {Type=T_Link "Responses" ; Markup=Input };
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(Deref("ResponseID",Column "Answer"))) };                                                                             
                                        ]}
                  ]}


  let DARE =
      {Name = "DARE";
        Tables = ["Participants",  {Columns=["Ability"   , {Type=T_Real           ; Markup=Latent(Gen(abilityPrior))} ]};
                  "Questions", {Columns=["Answer"  , {Type=T_AnswerRange                  ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "TrueAnswer", {Type=T_AnswerRange                ; Markup=Input};
                                         "Training"   , {Type=T_Bool              ; Markup=Input};
                                         "Difficulty", {Type=T_Real               ; Markup=Latent(Gen(difficultyPrior))}; 
                                         "Discrimination", {Type=T_Real           ; Markup=Latent(Gen(discriminationPrior))}; // new    
                                        ]};
                  "QuestionsTrain", {Columns=
                                        [ "QuestionID", {Type=T_Link "Questions"  ; Markup=Input};
                                          "Answer"  , {Type=T_AnswerRange                 ; Markup=Observable(Gen(Deref("QuestionID",Column "Answer")))};   
                                        ]};
                  "Responses", {Columns=
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Training"   , {Type=T_Bool            ; Markup=Input};
                                         "Advantage",   {Type=T_Real            ; Markup=Latent(Gen(DampBackward(
                                                                                                        Minus(Deref("ParticipantID",Column "Ability"),
                                                                                                              Deref("QuestionID",Column "Difficulty")),
                                                                                                        dampingFactor)))};
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Column("Advantage"),
                                                                                                          Deref("QuestionID",Column "Discrimination")))) };                                      
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Latent(Gen(StrictIf(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                             Column("Guess")))) };
                                         "TrueAnswer", {Type=T_AnswerRange             ; Markup=Input};                                                                                 
                                        ]};
                   "ResponsesTrain", {Columns=
                                        ["ResponseID", {Type=T_Link "Responses" ; Markup=Input };
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(Deref("ResponseID",Column "Answer"))) };                                                                             
                                        ]}
                  ]}
  let DAREUI =
      {Name = "DARE";
        Tables = ["Participants",  {Columns=["Ability"   , {Type=T_Real           ; Markup=Latent(Gen(abilityPrior))} ]};
                  "Questions", {Columns=["Answer"  , {Type=T_AnswerRange                  ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
 //                                        "TrueAnswer", {Type=T_AnswerRange                ; Markup=Input};
//                                         "Training"   , {Type=T_Bool              ; Markup=Input};
                                         "Difficulty", {Type=T_Real               ; Markup=Latent(Gen(difficultyPrior))}; 
                                         "Discrimination", {Type=T_Real           ; Markup=Latent(Gen(discriminationPrior))}; // new    
                                        ]};
                  "QuestionsTrain", {Columns=
                                        [ "QuestionID", {Type=T_Link "Questions"  ; Markup=Input};
                                          "Answer"  , {Type=T_AnswerRange                 ; Markup=Observable(Gen(Deref("QuestionID",Column "Answer")))};   
                                        ]};
                  "Responses", {Columns=
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input};
   //                                      "Training"   , {Type=T_Bool            ; Markup=Input};
                                         "Advantage",   {Type=T_Real            ; Markup=Latent(Gen(DampBackward(
                                                                                                        Minus(Deref("ParticipantID",Column "Ability"),
                                                                                                              Deref("QuestionID",Column "Difficulty")),
                                                                                                        dampingFactor)))};
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Column("Advantage"),
                                                                                                          Deref("QuestionID",Column "Discrimination")))) };                                      
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Latent(Gen(StrictIf(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                             Column("Guess")))) };
  //                                       "TrueAnswer", {Type=T_AnswerRange             ; Markup=Input};                                                                                 
                                        ]};
                   "ResponsesTrain", {Columns=
                                        ["ResponseID", {Type=T_Link "Responses" ; Markup=Input };
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(Deref("ResponseID",Column "Answer"))) };                                                                             
                                        ]}
                  ]}


module Models = 
  let mooc = [  MOOCEval.DA;
                MOOCEval.DAS;
                MOOCEval.DASG;
                MOOCEval.DASGC]

  let all = [ InfernoClassicMM.Schema;
              TrueSkillObservable.Schema;
              TrueSkill.Schema;
              TrueSkillQuery.Schema;
              LDAExample.Schema;
              Recommender.Schema;
              PureRecommender.Schema;
              PureRecommender.SchemaWithUpto;
              RecommenderQuery.Schema;
              ConjugateGaussian.Schema;
              GenGaussian.Schema;
              ConjugateGaussianWithLatent.Schema;
              GaussianWithPrior.Schema;
              DifficultyAbilityCG.Schema;
              DifficultyAbilityEvaluation.Schema;
              DifficultyAbility.Schema;
              DifficultyAbilityWCorrect.Schema;
              DifficultyAbilityDiscrete.Schema;
              DifficultyAbilityUntrainedDiscreteEvaluation.Schema;
              DifficultyAbilityDiscreteEvaluation.Schema;
              DifficultyAbilitySkipDiscreteEvaluation.Schema;
              DifficultyAbilitySkipGuessDiscreteEvaluation.Schema;
              DifficultyAbilitySkipGuessCheatDiscreteEvaluation.Schema;
              DifficultyAbilitySkip.Schema;
              DifficultyAbilitySkipCheat.Schema;
              //DARE.DARE;
              DAREQuery.DARE;
              LinearRegression.LR;
              LinearRegression.LRWithError;
              LinearRegression.LRWithSimpleError; ]

