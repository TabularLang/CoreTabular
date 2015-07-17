namespace MicrosoftResearch.Infer.Tabular


module NewModels = 
  open Syntax
  let abilityPrior    = MExp(Dist(GaussianFromMeanAndPrecision,[ Const(RealConst( 0.));Const(RealConst(1.))]))
  let difficultyPrior = MExp(Dist(GaussianFromMeanAndPrecision,[ Const(RealConst( 0.));Const(RealConst(1.))]))
  let discriminationPrior = MExp(Dist(GammaFromShapeAndScale,[ Const(RealConst( 5.0));Const(RealConst(0.2))])) //?
  let dampingFactor = Const(RealConst 0.2)
  let DB(e1,e2) = Prim(Factor(FactorName "DampBackward"),[e1;e2])
  let answerRange = 8
  let T_AnswerRange   = T_Upto (Const (IntConst answerRange))


  let DAConcrete () = //missing probit function 
      let T_AnswerRange   = T_Upto (Const (IntConst answerRange))
      [Declaration(Table("Participants", None), ["Ability"      , {Type=T_Real             ; Markup=Parsing.ParseMarkup("Latent(GaussianFromMeanAndPrecision(0.0,1.0))")}])
       Declaration(Table("Questions"   , None), ["Answer"       , {Type=T_AnswerRange      ; Markup=Parsing.ParseMarkup("Latent(DiscreteUniform(answerRange))")};
                              "Difficulty"   , {Type=T_Real                ; Markup=Parsing.ParseMarkup("Latent(GaussianFromMeanAndPrecision(0.0,1.0))")};
                              //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Input")};
                              //"Training"     , {Type=T_Bool                ; Markup=Parsing.ParseMarkup("Input")};
                              ])                                          
       Declaration(Table("QuestionsTrain", None),[ "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Parsing.ParseMarkup("Input")};
                              "Answer"       , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Output(QuestionID.Answer)") }
                              ]);                                         
       Declaration(Table("Responses",  None),    [ "ParticipantID", {Type=T_Link "Participants" ; Markup=Parsing.ParseMarkup("Input")};
                              "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Parsing.ParseMarkup("Input")}; 
                              //"Training"     , {Type=T_Bool                ; Markup=Parsing.ParseMarkup("Input")}; 
                              "Advantage"    , {Type=T_Real                ; Markup=Parsing.ParseMarkup("Latent(DampBackward(ParticipantID.Ability - QuestionID.Difficulty,0.2))") };
                              //"Know"         , {Type=T_Bool                ; Markup=Parsing.ParseMarkup("Latent(Probit(Advantage,1.0)") };
                              "Guess"        , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Latent(DiscreteUniform(answerRange))")};
                              "Answer"       , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Latent(if Know then QuestionID.Answer else Guess))")};
                              //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Input")};
                           ])
       Declaration(Table("ResponsesTrain", None),[ "ResponseID"   , {Type=T_Link "Responses"    ; Markup=Parsing.ParseMarkup("Input") };
                              "Answer"       , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Output(ResponseID.Answer)")};
                              ])
      ] 


  let DAREConcrete () = //missing probit function 
            let T_AnswerRange   = T_Upto (Const (IntConst answerRange))
            [Declaration(Table("Participants"   , None),["Ability"       , {Type=T_Real                ; Markup=Parsing.ParseMarkup("Latent(GaussianFromMeanAndPrecision(0.0,1.0))")}])
             Declaration(Table("Questions"      , None),["Answer"        , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Latent(DiscreteUniform(answerRange))")};
                                      "Difficulty"    , {Type=T_Real                ; Markup=Parsing.ParseMarkup("Latent(GaussianFromMeanAndPrecision(0.0,1.0))")};
                                      "Discrimination", {Type=T_Real                ; Markup=Parsing.ParseMarkup("Latent(Gamma(5.0, 0.2))")}; 
                                      //"TrueAnswer"    , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Input")};
                                      //"Training"      , {Type=T_Bool                ; Markup=Parsing.ParseMarkup("Input")};
                                       ])                                          
             Declaration(Table("QuestionsTrain",None), [ "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Parsing.ParseMarkup("Input")};
                                      "Answer"       , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Output(QuestionID.Answer)") }
                                       ]);                                         
             Declaration(Table("Responses",  None),    [ "ParticipantID", {Type=T_Link "Participants" ; Markup=Parsing.ParseMarkup("Input")};
                                      "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Parsing.ParseMarkup("Input")}; 
                                      //"Training"     , {Type=T_Bool                ; Markup=Parsing.ParseMarkup("Input")}; 
                                      "Advantage"    , {Type=T_Real                ; Markup=Parsing.ParseMarkup("Latent(DampBackward(ParticipantID.Ability - QuestionID.Difficulty,0.2))") };
                                      //"Know"         , {Type=T_Bool                ; Markup=Parsing.ParseMarkup("Latent(Probit(Advantage,QuestionID.Discrimination)") };
                                      "Guess"        , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Latent(DiscreteUniform(answerRange))")};
                                      "Answer"       , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Latent(if Know then QuestionID.Answer else Guess))")};
                                      //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Input")};
                                    ])
             Declaration(Table("ResponsesTrain", None),[ "ResponseID"   , {Type=T_Link "Responses"    ; Markup=Parsing.ParseMarkup("Input") };
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Output(ResponseID.Answer)")};
                                     ])
            ] 
  
  let Ability =  
            [Declaration(Table("Participants"   , None),["Ability"      , {Type=T_Real                ; Markup=Latent(abilityPrior)}])
             Declaration(Table("Questions"      ,None), ["Answer"       , {Type=T_AnswerRange         ; Markup=Latent(MExp(Dist(DiscreteUniform, [(Const (IntConst answerRange))])))};
                                       //"Difficulty"   , {Type=T_Real                ; Markup=Latent(difficultyPrior)};
                                       //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Input};
                                       //"Training"     , {Type=T_Bool                ; Markup=Input};
                                        ])                                          
             Declaration(Table("QuestionsTrain", None),[ "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Input};
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Observable(MExp(DeRef(Var("QuestionID"),"Questions","Answer"))) }
                                        ]);                                         
             Declaration(Table("Responses",  None),    [ "ParticipantID", {Type=T_Link "Participants" ; Markup=Input};
                                       "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Input}; 
                                       //"Training"     , {Type=T_Bool                ; Markup=Input}; 
                                       "Advantage"    , {Type=T_Real                ; Markup=Latent(MExp(DB((DeRef(Var "ParticipantID","Participants", "Ability"),dampingFactor)))) };
                                       "Know"         , {Type=T_Bool                ; Markup=Latent(MExp(Prim(Factor(FactorName "Probit"), 
                                                                                                                     [Var "Advantage";Const(RealConst 1.0) ]))) };
                                       "Guess"        , {Type=T_AnswerRange         ; Markup=Latent(MExp(Dist(DiscreteUniform, [(Const (IntConst answerRange))])))};
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Latent(MExp(If(Var "Know", DeRef(Var "QuestionID","Questions","Answer"), Var("Guess"))))};
                                        //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Input};
                                     ]);
              Declaration(Table("ResponsesTrain",None), [ "ResponseID"   , {Type=T_Link "Responses"    ; Markup=Input };
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Observable(MExp(DeRef(Var "ResponseID","Responses", "Answer")))};
                                     ])
            ] 

  let DA =  [Declaration(Table("Participants"   ,None), ["Ability"      , {Type=T_Real                ; Markup=Latent(abilityPrior)}])
             Declaration(Table("Questions"      ,None), ["Answer"       , {Type=T_AnswerRange         ; Markup=Latent(MExp(Dist(DiscreteUniform, [(Const (IntConst answerRange))])))};
                                       "Difficulty"   , {Type=T_Real                ; Markup=Latent(difficultyPrior)};
                                       //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Input};
                                       //"Training"     , {Type=T_Bool                ; Markup=Input};
                                        ])                                          
             Declaration(Table("QuestionsTrain",None), [ "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Input};
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Observable(MExp(DeRef(Var("QuestionID"),"Questions","Answer"))) }
                                        ]);                                         
             Declaration(Table("Responses",  None),    [ "ParticipantID", {Type=T_Link "Participants" ; Markup=Input};
                                       "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Input}; 
                                       //"Training"     , {Type=T_Bool                ; Markup=Input}; 
                                       "Advantage"    , {Type=T_Real                ; Markup=Latent(MExp(DB((Prim(Minus,
                                                                                                                 [ DeRef(Var "ParticipantID","Participants", "Ability")
                                                                                                                   DeRef(Var "QuestionID"   , "Questions"  , "Difficulty") ])),dampingFactor)))};
                                       "Know"         , {Type=T_Bool                ; Markup=Latent(MExp(Prim(Factor(FactorName "Probit"), 
                                                                                                                     [Var "Advantage";Const(RealConst 1.0) ]))) };
                                       "Guess"        , {Type=T_AnswerRange         ; Markup=Latent(MExp(Dist(DiscreteUniform, [(Const (IntConst answerRange))])))};
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Latent(MExp(If(Var "Know", DeRef(Var "QuestionID","Questions","Answer"), Var("Guess"))))};
                                        //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Input};
                                     ])
             Declaration(Table("ResponsesTrain", None),[ "ResponseID"   , {Type=T_Link "Responses"    ; Markup=Input };
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Observable(MExp(DeRef(Var "ResponseID","Responses", "Answer")))};
                                     ])
            ] 

  let DARE = 
            [Declaration(Table("Participants"   ,None), ["Ability"      , {Type=T_Real                ; Markup=Latent(abilityPrior)}])
             Declaration(Table("Questions"      ,None), ["Answer"       , {Type=T_AnswerRange         ; Markup=Latent(MExp(Dist(DiscreteUniform, [(Const (IntConst answerRange))])))};
                                       "Difficulty"   , {Type=T_Real                ; Markup=Latent(difficultyPrior)};
                                       "Discrimination", {Type=T_Real                ; Markup=Latent(discriminationPrior)};
                                       //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Input};
                                       //"Training"     , {Type=T_Bool                ; Markup=Input};
                                        ])                                          
             Declaration(Table("QuestionsTrain",None), [ "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Input};
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Observable(MExp(DeRef(Var("QuestionID"),"Questions","Answer"))) }
                                        ]);                                         
             Declaration(Table("Responses",   None),   [ "ParticipantID", {Type=T_Link "Participants" ; Markup=Input};
                                       "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Input}; 
                                       //"Training"     , {Type=T_Bool                ; Markup=Input}; 
                                       "Advantage"    , {Type=T_Real                ; Markup=Latent(MExp(DB((Prim(Minus,
                                                                                                                 [ DeRef(Var "ParticipantID","Participants", "Ability")
                                                                                                                   DeRef(Var "QuestionID"   , "Questions"  , "Difficulty") ])),dampingFactor)))};
                                       "Know"         , {Type=T_Bool                ; Markup=Latent(MExp(Prim(Factor(FactorName "Probit"), 
                                                                                                                     [Var "Advantage";DeRef(Var "QuestionID"   , "Questions"  , "Discrimination") ]))) };
                                       "Guess"        , {Type=T_AnswerRange         ; Markup=Latent(MExp(Dist(DiscreteUniform, [(Const (IntConst answerRange))])))};
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Latent(MExp(If(Var "Know", DeRef(Var "QuestionID","Questions","Answer"), Var("Guess"))))};
                                        //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Input};
                                     ])
             Declaration(Table("ResponsesTrain",None), [ "ResponseID"   , {Type=T_Link "Responses"    ; Markup=Input };
                                       "Answer"       , {Type=T_AnswerRange         ; Markup=Observable(MExp(DeRef(Var "ResponseID","Responses", "Answer")))};
                                     ])
            ] 


  let DAREProgression = [ ("A",Ability); ("DA", DA); ("DARE", DARE) ]
