namespace MicrosoftResearch.Infer.Tabular

open Tabular

                  
module MOOCEval =
  let alreadyKnewNoise = RealConst 1.0
  let dampingRate = RealConst 0.6
  let DB(e)=DampBackward(e,dampingRate)
  let T_Int = T_Upto(Const 7)
  let DA =
       { Name = "MOOCEval.DifficultyAbility";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]}
                  "Questions", {Columns=[
                                         "answerIdx"  , {Type=T_Int               ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(DB(Deref("studentID",Column "ability")), DB(Deref("questionID",Column "difficulty"))),alreadyKnewNoise))) }; 
                                         "guess"     , {Type=T_Int              ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};
                                         "answerIdx" , {Type=T_Int              ; Markup=Observable(Gen(If(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                           Column "guess" )))};                                                                                                                       
                                     ]};                                                                                            
                  "Responses", 
                                    {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Training"  , {Type=T_Bool             ; Markup=Input};
                                         "answerIdx" , {Type=T_Int              ; Markup=Input};
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(DB(Deref("studentID",Column "ability")), DB(Deref("questionID",Column "difficulty"))),alreadyKnewNoise))) }; 
                                         "guess"     , {Type=T_Int              ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};  
                                         "answerIdxP" , {Type=T_Int             ; Markup=Latent(Gen(If(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                                  Column("guess"))
                                                                                                                           )) };
                                         ]};
                  ]}
  let skipNoise = RealConst 10.0
  let DAS =
      { Name = "MOOCEval.DifficultyAbilitySkip";
        Tables = ["Students",  {Columns=["ability"   ,    {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))};
                                         "skipPropensity",{Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst -1.3, RealConst 3.3)))};
                                        ]}
                  "Questions", {Columns=[
                                         "answerIdx"  , {Type=T_Int               ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(DB(Deref("studentID",Column "skipPropensity")),skipNoise))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(DB(Deref("studentID",Column "ability")), DB(Deref("questionID",Column "difficulty"))),alreadyKnewNoise))) }; 
                                         "guess"     , {Type=T_Int              ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};
                                         "answerIdx" , {Type=T_Int              ; Markup=Observable(Gen(If(Column("skipped"),DiscreteConst (0,7),
                                                                                                           If(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                              Column "guess" ))))};                                                                                                                       
                                     ]};                                                                                            
                  "Responses", 
                                    {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Training"  , {Type=T_Bool             ; Markup=Input};
                                         "answerIdx" , {Type=T_Int              ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(DB(Deref("studentID",Column "skipPropensity")),skipNoise))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(DB(Deref("studentID",Column "ability")), DB(Deref("questionID",Column "difficulty"))),alreadyKnewNoise))) }; 
                                         "guess"     , {Type=T_Int              ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};
                                         "answerIdxP" , {Type=T_Int             ; Markup=Latent(Gen(If(Column("skipped"),DiscreteConst (0,7),
                                                                                                           If(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                              Column "guess" ))))};                                                                                                                       
                                         ]};
                  ]}

  let DASG =
      { Name = "MOOCEval.DifficultyAbilitySkipGuess";
        Tables = ["Students",  {Columns=["ability"   ,    {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))};
                                         "skipPropensity",{Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst -1.3, RealConst 3.3)))};
                                        ]}
                  "Questions", {Columns=[
                                         "answerIdx"  , {Type=T_Int               ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(DB(Deref("studentID",Column "skipPropensity")),skipNoise))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(DB(Deref("studentID",Column "ability")), DB(Deref("questionID",Column "difficulty"))),alreadyKnewNoise))) }; 
                                         "guess"     , {Type=T_Int              ; Markup=Latent(CDiscreteWith(Const 7))}; 
                                         "answerIdx" , {Type=T_Int              ; Markup=Observable(Gen(If(Column("skipped"),
                                                                                                           DiscreteConst (0,7),
                                                                                                           If(Column("know"), 
                                                                                                              Deref("questionID",Column "answerIdx"), 
                                                                                                              Column "guess" ))))};                                                                                                                       
                                     ]};                                                                                            
                  "Responses", 
                                    {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Training"  , {Type=T_Bool             ; Markup=Input};
                                         "answerIdx" , {Type=T_Int              ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(DB(Deref("studentID",Column "skipPropensity")),skipNoise))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(DB(Deref("studentID",Column "ability")), DB(Deref("questionID",Column "difficulty"))),alreadyKnewNoise))) }; 
                                         "guess"     , {Type=T_Int              ; Markup=Latent(WithPrior(CDiscreteWith(Const 7),Param("ResponsesTrain","guess")))};
                                          "answerIdxP" , {Type=T_Int             ; Markup=Latent(Gen(If(Column("skipped"),DiscreteConst (0,7),
                                                                                                       If(Column("know"), 
                                                                                                             Deref("questionID",Column "answerIdx"), 
                                                                                                              Column "guess" ))))};                                                                                                                       
                                         ]};
                  ]}

  let DASGC =
      { Name = "MOOCEval.DifficultyAbilitySkipGuessCheat";
        Tables = ["Students",  {Columns=["ability"   ,    {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))};
                                         "skipPropensity",{Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst -1.3, RealConst 3.3)))};
                                         "cheats"   , {Type=T_Bool                   ; Markup=Latent(Gen(Bernoulli(RealConst 0.5)))} //TODO check prior
                                        ]}
                  "Questions", {Columns=[
                                         "answerIdx"  , {Type=T_Int               ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(DB(Deref("studentID",Column "skipPropensity")),skipNoise))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(DB(Deref("studentID",Column "ability")), DB(Deref("questionID",Column "difficulty"))),alreadyKnewNoise))) }; 
                                        "guess"     , {Type=T_Int              ; Markup=Latent(CDiscreteWith(Const 7))}; //
                                         "answerIdx" , {Type=T_Int              ; Markup=Observable(Gen(If(Column("skipped"),
                                                                                                           DiscreteConst (0,7),
                                                                                                           If(Or(Deref("studentID",Column "cheats"),Column("know")), 
                                                                                                              Deref("questionID",Column "answerIdx"), 
                                                                                                              Column "guess" ))))};                                                                                                                       
                                     ]};                                                                                            
                  "Responses", 
                                    {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Training"  , {Type=T_Bool             ; Markup=Input};
                                         "answerIdx" , {Type=T_Int              ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(DB(Deref("studentID",Column "skipPropensity")),skipNoise))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(DB(Deref("studentID",Column "ability")), DB(Deref("questionID",Column "difficulty"))),alreadyKnewNoise))) }; 
                                         "guess"     , {Type=T_Int              ; Markup=Latent(WithPrior(CDiscreteWith(Const 7),Param("ResponsesTrain","guess")))};
                                         "answerIdxP" , {Type=T_Int             ; Markup=Latent(Gen(If(Column("skipped"),DiscreteConst (0,7),
                                                                                                       If(Or(Deref("studentID",Column "cheats"),Column("know")), 
                                                                                                             Deref("questionID",Column "answerIdx"), 
                                                                                                              Column "guess" ))))};                                                                                                                       
                                         ]};
                  ]}
