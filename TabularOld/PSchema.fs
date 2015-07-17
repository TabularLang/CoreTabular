namespace MicrosoftResearch.Infer.Tabular

open Tabular

module InfernoClassicMM =
  open Tabular
  let Schema =
    {Name = "InfernoClassicMM";
    Tables = [ "InfernoClassicMM",
                 {Columns=["Z",       {Type=(* T_Int *) T_Upto(Const 3); Markup=Latent(CDiscreteWith(Const 3))};
                           "X0",      {Type=T_Real; Markup=Latent(Array(CGaussian,[Column "Z"]))};
                           "X1",      {Type=T_Real; Markup=Latent(Array(CGaussian,[Column "Z"]))};
                           "X2",      {Type=T_Real; Markup=Latent(Array(CGaussian,[Column "Z"]))};
                           ]};
               "X0",
                 {Columns=["R",{Type=T_Link "InfernoClassicMM"; Markup=Input}
                           "X0",{Type=T_Real; Markup=Observable(Gen(Deref("R",Column "X0")))}
                           ]};
               "X1",
                 {Columns=["R",{Type=T_Link "InfernoClassicMM"; Markup=Input}
                           "X1",{Type=T_Real; Markup=Observable(Gen(Deref("R",Column "X1")))}
                           ]};
               "X2",
                 {Columns=["R",{Type=T_Link "InfernoClassicMM"; Markup=Input}
                           "X2",{Type=T_Real; Markup=Observable(Gen(Deref("R",Column "X2")))}
                           ]};                               
             ]}


module TrueSkillObservable =
  open Tabular

  let Schema =
    {Name = "TrueSkillObservable";
    Tables = [ "Players",  {Columns=["Skill",        {Type=T_Real; Markup=Latent(Gen(Gaussian(RealConst 25., RealConst 0.01)))} ] };
               "Matches",  {Columns=["Player1",      {Type=T_Link "Players"; Markup=Observable(CDiscreteWith(SizeOf "Players"))};
                                     "Player2",      {Type=T_Link "Players"; Markup=Observable(CDiscreteWith(SizeOf "Players"))};
                                     "Perf1",        {Type=T_Real; Markup=Latent(Gen(Gaussian(Deref("Player1", Column "Skill"), RealConst 1.0)))};
                                     "Perf2",        {Type=T_Real; Markup=Latent(Gen(Gaussian(Deref("Player2", Column "Skill"), RealConst 1.0)))};
                                     "Player1Wins",  {Type=T_Bool; Markup=Observable(Gen(GT(Column "Perf1", Column "Perf2")))} ]} ]}

module TrueSkill =
  open Tabular
  let Schema =
    {Name = "TrueSkillInput";
    Tables = [ "Players",  {Columns=["Skill",        {Type=T_Real; Markup=Latent(Gen(Gaussian(RealConst 25., RealConst 0.01)))} ] };
               "Matches",  {Columns=["Player1",      {Type=T_Link "Players"; Markup=Input};
                                     "Player2",      {Type=T_Link "Players"; Markup=Input};
                                     "Perf1",        {Type=T_Real; Markup=Latent(Gen(Gaussian(Deref("Player1", Column "Skill"), RealConst 1.0)))};
                                     "Perf2",        {Type=T_Real; Markup=Latent(Gen(Gaussian(Deref("Player2", Column "Skill"), RealConst 1.0)))};
                                     "Player1Wins",  {Type=T_Bool; Markup=Observable(Gen(GT(Column "Perf1", Column "Perf2")))} ]} ]}
module TrueSkillQuery =
  open Tabular
  let Schema =
    {Name = "TrueSkillQuery";
    Tables = [ "Players",  {Columns=["Skill",        {Type=T_Real; Markup=Latent(Gen(Gaussian(RealConst 25., RealConst 0.01)))} ] };
               "Matches",  {Columns=["Player1",      {Type=T_Link "Players"; Markup=Input};
                                     "Player2",      {Type=T_Link "Players"; Markup=Input};
                                     "Perf1",        {Type=T_Real; Markup=Latent(Gen(Gaussian(Deref("Player1", Column "Skill"), RealConst 1.0)))};
                                     "Perf2",        {Type=T_Real; Markup=Latent(Gen(Gaussian(Deref("Player2", Column "Skill"), RealConst 1.0)))};
                                     "Player1Wins",  {Type=T_Bool; Markup=Observable(Gen(GT(Column "Perf1", Column "Perf2")))} ]} 
               "HypotheticalMatches", 
                           {Columns=["Player1",      {Type=T_Link "Players"; Markup=Input};
                                     "Player2",      {Type=T_Link "Players"; Markup=Input};
                                     "Perf1",        {Type=T_Real; Markup=Latent(Gen(Gaussian(Deref("Player1", Column "Skill"), RealConst 1.0)))};
                                     "Perf2",        {Type=T_Real; Markup=Latent(Gen(Gaussian(Deref("Player2", Column "Skill"), RealConst 1.0)))};
                                     "Player1Wins",  {Type=T_Bool; Markup=Latent(Gen(GT(Column "Perf1", Column "Perf2")))} ]} 
            ]}

                         
module LDAExample =
 open Tabular
 let NumberTopics = 20
 let Schema =
    {Name = "LDA";
    Tables = [ "Words",       {Columns=["Word",     {Type=T_String;       Markup=Input} ]};
                "Docs",        {Columns=["Filename", {Type=T_String;       Markup=Input} ]};
                "Occurrences", {Columns=["DocID",    {Type=T_Link "Docs";  Markup=Input};
                                        "Position", {Type=T_Int;          Markup=Input};
                                        "Topic",    {Type=(* T_Int*) T_Upto(Const NumberTopics);          Markup=Latent(Array(CDiscreteWith(Const NumberTopics), [Column "DocID"]))};
                                        "WordID",   {Type=T_Link "Words"; Markup=Observable(Array(CDiscreteWith(SizeOf "Words"), [Column "Topic"]))} ]} ]}
                                    
module Recommender =
  open Tabular

  let Schema =
      {Name = "MovieRatingsObservable";
        Tables = [ "Users",   {Columns=["UserCluster",   {Type=T_Upto (Const 4);          Markup=Latent(CDiscreteWith(Const 4))};
                                        "IsMale",        {Type=T_Bool;         Markup=Observable(Array(CBernoulli, [Column "UserCluster"]))};
                                        "Age",           {Type=T_Upto(Const 4);          Markup=Observable(Array(CDiscreteWith(Const 100), [Column "UserCluster"]))} ]}
                   "Movies",  {Columns=["MovieCluster",  {Type=T_Upto(Const 4);          Markup=Latent(CDiscreteWith(Const 4))};
                                        "Category",      {Type=T_Upto(Const 20);          Markup=Observable(Array(CDiscreteWith(Const 20), [Column "MovieCluster"])) };
                                        "Year",          {Type=T_Upto(Const 100);          Markup=Observable(Array(CDiscreteWith(Const 100), [Column "MovieCluster"])) } ]};
                   "Ratings", {Columns=["UserID",        {Type=T_Link "Users"; Markup=Observable(CDiscreteWith(SizeOf "Users"))};
                                        "MovieID",       {Type=T_Link "Movies";Markup=Observable(CDiscreteWith(SizeOf "Movies"))};
                                        "Rating",        {Type=T_Upto(Const 5);          Markup=Observable(Array(CDiscreteWith(Const 5), [Deref("UserID", Column "UserCluster"); Deref("MovieID", Column "MovieCluster")])) }]} ]}


module PureRecommender =
  open Tabular
  let Schema =
      {Name = "MovieRatingsInput";
        Tables = [ "Users",   {Columns=["UserCluster",   {Type=T_Upto(Const 4);        Markup=Latent(CDiscreteWith(Const 4))};
                                      "IsMale",        {Type=T_Bool;         Markup=Observable(Array(CBernoulli,[Column "UserCluster"]))};
                                      "Age",           {Type=T_Int;          Markup=Observable(Array(CDiscreteWith(Const 100),[Column "UserCluster"]))} ]}
                   "Movies",  {Columns=["MovieCluster",  {Type=T_Upto(Const 4);        Markup=Latent(CDiscreteWith(Const 4))};
                                      "Category",      {Type=T_Upto(Const 20);          Markup=Observable(Array(CDiscreteWith(Const 20),[Column "MovieCluster"])) };
                                      "Year",          {Type=T_Upto(Const 100);          Markup=Observable(Array(CDiscreteWith(Const 100),[Column "MovieCluster"])) } ]};
                   "Ratings", {Columns=["UserID",        {Type=T_Link "Users";  Markup=Input};
                                      "MovieID",       {Type=T_Link "Movies"; Markup=Input};
                                      "Rating",         {Type=T_Upto(Const 5);          Markup=Observable(Array(CDiscreteWith(Const 5), [Deref("UserID", Column "UserCluster"); 
                                                                                                                               Deref("MovieID", Column "MovieCluster")])) }]} ]}

  let SchemaWithUpto =
      {Name = "MovieRatingsWithUpto";
        Tables = [ "Users",   {Columns=["UserCluster",   {Type=T_Upto(Const 4);          Markup=Latent(CDiscreteWith(Const 4))};
                                        "IsMale",        {Type=T_Bool;                   Markup=Observable(Array(CBernoulli,[Column "UserCluster"]))};
                                        "Age",           {Type=T_Upto(Const 100);        Markup=Observable(Array(CDiscreteWith(Const 100),[Column "UserCluster"]))} ]}
                   "Movies",  {Columns=["MovieCluster",  {Type=T_Upto(Const 4);          Markup=Latent(CDiscreteWith(Const 4))};
                                        "Category",      {Type=T_Upto(Const 20);         Markup=Observable(Array(CDiscreteWith(Const 20),[Column "MovieCluster"])) };
                                        "Year",          {Type=T_Upto(Const 100);        Markup=Observable(Array(CDiscreteWith(Const 100),[Column "MovieCluster"])) } ]};
                   "Ratings", {Columns=["UserID",        {Type=T_Link "Users";           Markup=Input};
                                        "MovieID",       {Type=T_Link "Movies";          Markup=Input};
                                        "Rating",         {Type=T_Upto(Const 6);         Markup=Observable(Array(CDiscreteWith(Const 5), [Deref("UserID", Column "UserCluster"); 
                                                                                                                                          Deref("MovieID", Column "MovieCluster")])) }]}
                                      //"Rating",         {Type=T_Upto(Const 5);          Markup=Observable(Array(CDiscreteWith(Const 5), [Deref("UserID", Column "UserCluster")])) }]} 
     
      ]}


module RecommenderQuery =
  open Tabular
  let Schema =
      {Name = "MovieRatingsQuery";
        Tables = [ "Users",   {Columns=["UserCluster",   {Type=T_Upto(Const 4);          Markup=Latent(CDiscreteWith(Const 4))};
                                        "IsMale",        {Type=T_Bool;         Markup=Observable(CBernoulli)};
                                        "Age",           {Type=T_Upto(Const 100);          Markup=Observable(CDiscreteWith(Const 100))} ]}
                   "Movies",  {Columns=["MovieCluster",  {Type=T_Upto(Const 4);          Markup=Latent(CDiscreteWith(Const 4))};
                                        "Category",      {Type=T_Upto(Const 20);          Markup=Observable(CDiscreteWith(Const 20)) };
                                        "Year",          {Type=T_Upto(Const 100);          Markup=Observable(CDiscreteWith(Const 100)) } ]};
                   "Ratings", {Columns=["UserID",        {Type=T_Link "Users";  Markup=Input};
                                        "MovieID",       {Type=T_Link "Movies"; Markup=Input};
                                        "Rating",        {Type=T_Upto(Const 5);          Markup=Observable(Array(CDiscreteWith(Const 5), [Deref("UserID", Column "UserCluster"); Deref("MovieID", Column "MovieCluster")])) }]}
                   "RatingsQuery",
                              {Columns=["UserID",        {Type=T_Link "Users";  Markup=Input};
                                        "MovieID",       {Type=T_Link "Movies"; Markup=Latent(Gen(DiscreteUniform(SizeOf("Movies"))))};
                                        "Rating",        {Type=T_Upto(Const 5);           Markup=Observable(
                                                                                       WithPrior(Array(CDiscreteWith(Const 5), [Deref("UserID", Column "UserCluster"); Deref("MovieID", Column "MovieCluster")]),
                                                                                                 Param("Ratings","Rating"))) }]}   
                 ]}


module ConjugateGaussian =
  open Tabular
  let Schema =
      {Name = "ConjugateGaussian";
        Tables = ["Sample",  {Columns=["gsample"   , {Type=T_Real                       ; Markup=Observable(CGaussian)}; ]} ]}

module GenGaussian =
  open Tabular
  let Schema =
      {Name = "GenGaussian";
        Tables = ["Sample",  {Columns=["gsample"   , {Type=T_Real                       ; Markup=Observable((Gen(Gaussian(RealConst 0., RealConst 1.))))}; ]} ]}

module ConjugateGaussianWithLatent =
  open Tabular
  let Schema =
      {Name = "ConjugateGaussianWithLatent";
        Tables = ["Sample",  {Columns=["gsample"   , {Type=T_Real                       ; Markup=Observable(CGaussian)};
                                       "lsample"   , {Type=T_Real                       ; Markup=Latent(CGaussian)} ]} ]}

module GaussianWithPrior =
  open Tabular
  let Schema =
      {Name = "GaussianWithPrior";
        Tables = ["Sample",  {Columns=["gsample"   , {Type=T_Real                       ; Markup=Observable(CGaussian)};
                                       "lsample"   , {Type=T_Real                       ; Markup=Latent(WithPrior(CGaussian, Column("$gsample")))} ]} ]}

(* This version inlines the answers array into the model for Responses.answer 
module DifficultyAbility =
  open Tabular
  let Schema =
      {Name = "DifficultyAbility";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                       ; Markup=Latent(CGaussian)} ]}
                  "Answers",   {Columns=["text"      , {Type=T_String                     ; Markup=Input}; ]};
                  "Questions", {Columns=["text"      , {Type=T_String                     ; Markup=Input};
                                          "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer2"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer3"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer4"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer5"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "difficulty", {Type=T_Real                      ; Markup=Latent(CGaussian)};         
                                        ]};
                  "Responses", {Columns=[ "studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen( Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty"))))) };
                                          "guess"     , {Type=T_Int              ; Markup=Latent(Gen(DiscreteUniform(Const 7)))};
                                          //NB: need to use StrictIf to avoid AllZeroException bug encountered with If - not sure why
                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(StrictIf(Column("know"), Deref("questionID",Column "answer"),
                                                                                                                                  Index(Arr [|  Const 0;
                                                                                                                                                Deref("questionID",Column "answer0");
                                                                                                                                                Deref("questionID",Column "answer1");
                                                                                                                                                Deref("questionID",Column "answer2");
                                                                                                                                                Deref("questionID",Column "answer3"); 
                                                                                                                                                Deref("questionID",Column "answer4"); 
                                                                                                                                                Deref("questionID",Column "answer5") |],
                                                                                                                                        Column("guess")))
                                                                                                          )) }; 
                                          "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer"))))  }; 
                                          ]} ]}

*)

module DifficultyAbilityCG =
  open Tabular
  let Schema =
      {Name = "DifficultyAbilityCG";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                       ; Markup=Latent(CGaussian)} ]}
                  "Answers",   {Columns=["text"      , {Type=T_String                     ; Markup=Input}; ]};
                  "Questions", {Columns=["text"      , {Type=T_String                     ; Markup=Input};
                                          "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer2"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer3"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer4"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer5"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answers"   , {Type=T_Array(T_Link "Answers",Const 7)  
                                                                                          ; Markup=Latent(Gen(Arr
                                                                                                               [| Const 0; //NB: this had better agree with DB ID 1
                                                                                                                  Column "answer0";
                                                                                                                  Column "answer1";
                                                                                                                  Column "answer2";
                                                                                                                  Column "answer3"; 
                                                                                                                  Column "answer4"; 
                                                                                                                  Column "answer5";
                                                                                                               |]))};
                                          "difficulty", {Type=T_Real                      ; Markup=Latent(CGaussian)};         
                                        ]};
                  "Responses", {Columns=[ "studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty")), RealConst 1.0))) };
                                          "guess"     , {Type=T_Upto (Const 7)             ; Markup=Latent(Gen(DiscreteUniform(Const 7)))};
                                          //NB: need to use StrictIf to avoid AllZeroException bug encountered with If - not sure why
                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(StrictIf(Column("know"), Deref("questionID",Column "answer"),
                                                                                                                                  Index(Deref("questionID",Column "answers"),
                                                                                                                                        Column("guess")))
                                                                                                          )) }; 
                                          "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer"))))  }; 
                                          ]} ]}
module DifficultyAbilityEvaluation =
  open Tabular
  let Schema =
      {Name = "DifficultyAbilityEvaluation";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                       ; Markup=Latent(CGaussian)} ]}
                  "Answers",   {Columns=["text"      , {Type=T_String                     ; Markup=Input}; ]};
                  "Questions", {Columns=["text"      , {Type=T_String                     ; Markup=Input};
                                          "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer2"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer3"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer4"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer5"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answers"   , {Type=T_Array(T_Link "Answers", Const 7)   
                                                                                          ; Markup=Latent(Gen(Arr
                                                                                                               [| Const 0; //NB: this had better agree with DB ID 1
                                                                                                                  Column "answer0";
                                                                                                                  Column "answer1";
                                                                                                                  Column "answer2";
                                                                                                                  Column "answer3"; 
                                                                                                                  Column "answer4"; 
                                                                                                                  Column "answer5";
                                                                                                               |]))};
                                          "difficulty", {Type=T_Real                      ; Markup=Latent(CGaussian)};         
                                        ]};
                  "ResponsesTraining", 
                               {Columns=[ "studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen( Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")), RealConst 1.0))) };
                                          "guess"     , {Type=T_Upto (Const 7)            ; Markup=Latent(Gen(DiscreteUniform(Const 7)))};
                                          //NB: need to use StrictIf to avoid AllZeroException bug encountered with If - not sure why
                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(StrictIf(Column("know"), Deref("questionID",Column "answer"),
                                                                                                                                  Index(Deref("questionID",Column "answers"),
                                                                                                                                        Column("guess")))
                                                                                                          )) }; 
                                          "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer"))))  }; 
                                          ]} 
                  "ResponsesTest", 
                               {Columns=[ "studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen( Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty")), RealConst 1.0))) };
                                          "guess"     , {Type=T_Upto (Const 7)              ; Markup=Latent(Gen(DiscreteUniform(Const 7)))};
                                          //NB: need to use StrictIf to avoid AllZeroException bug encountered with If - not sure why
                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(StrictIf(Column("know"), Deref("questionID",Column "answer"),
                                                                                                                                  Index(Deref("questionID",Column "answers"),
                                                                                                                                        Column("guess")))
                                                                                                          )) }; 
                                          "correct"   , {Type=T_Bool             ; Markup=Latent(Gen(EQ(Column("answer"), Deref("questionID",Column "answer"))))  }; 
                                          ]} 
                  ]}

module DifficultyAbility =
  open Tabular
  let Schema =
      {Name = "DifficultyAbility";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                       ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]}
                  "Answers",   {Columns=["text"      , {Type=T_String                     ; Markup=Input}; ]};
                  "Questions", {Columns=["text"      , {Type=T_String                     ; Markup=Input};
                                          "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer2"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer3"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer4"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer5"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answers"   , {Type=T_Array(T_Link "Answers", Const 7)   
                                                                                          ; Markup=Latent(Gen(Arr
                                                                                                               [| Const 0; //NB: this had better agree with DB ID 1
                                                                                                                  Column "answer0";
                                                                                                                  Column "answer1";
                                                                                                                  Column "answer2";
                                                                                                                  Column "answer3"; 
                                                                                                                  Column "answer4"; 
                                                                                                                  Column "answer5";
                                                                                                               |]))};
                                          "difficulty", {Type=T_Real                      ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=[ "studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty")),RealConst 1.0))) };
                                          "guess"     , {Type=T_Upto(Const 7)             ; Markup=Latent(Gen(DiscreteUniform(Const 7)))};
                                          //NB: need to use StrictIf to avoid AllZeroException bug encountered with If - not sure why
                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(StrictIf(Column("know"), Deref("questionID",Column "answer"),
                                                                                                                                  Index(Deref("questionID",Column "answers"),
                                                                                                                                        Column("guess")))
                                                                                                          )) }; 
                                          //"correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer"))))  }; 
                                          ]} ]}


module DifficultyAbilityWCorrect =
  open Tabular
  let Schema =
      {Name = "DifficultyAbilityWCorrect";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                       ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]}
                  "Answers",   {Columns=["text"      , {Type=T_String                     ; Markup=Input}; ]};
                  "Questions", {Columns=["text"      , {Type=T_String                     ; Markup=Input};
                                          "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer2"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer3"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer4"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer5"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answers"   , {Type=T_Array(T_Link "Answers", Const 7)  
                                                                                          ; Markup=Latent(Gen(Arr
                                                                                                               [| Const 0; //NB: this had better agree with DB ID 1
                                                                                                                  Column "answer0";
                                                                                                                  Column "answer1";
                                                                                                                  Column "answer2";
                                                                                                                  Column "answer3"; 
                                                                                                                  Column "answer4"; 
                                                                                                                  Column "answer5";
                                                                                                               |]))};
                                          "difficulty", {Type=T_Real                      ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=[ "studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen( Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty")), RealConst 1.0))) };
                                          "guess"     , {Type=T_Upto(Const 7)    ; Markup=Latent(Gen(DiscreteUniform(Const 7)))};
                                          //NB: need to use StrictIf to avoid AllZeroException bug encountered with If - not sure why
                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(StrictIf(Column("know"), Deref("questionID",Column "answer"),
                                                                                                                                  Index(Deref("questionID",Column "answers"),
                                                                                                                                        Column("guess")))
                                                                                                          )) }; 
                                          "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer"))))  }; 
                                          ]} ]}

module DifficultyAbilityDiscrete =
  let Schema =
      {Name = "DifficultyAbilityDiscrete";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]};
                  "Questions", {Columns=["answerIdx"  , {Type=T_Upto (Const 7)              ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")),RealConst 1.0))) };
                                         "guess"     , {Type=T_Upto (Const 7)             ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};
                                         "answerIdx" , {Type=T_Upto (Const 7)            ; Markup=Observable(Gen(StrictIf(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                                 Column("guess")))) };
                                                                                                                          
                                        ]}
                  ]}


module DifficultyAbilityUntrainedDiscreteEvaluation =
  let Schema =
      { Name = "DifficultyAbilityUntrainedDiscreteEvaluation";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]}
                  "Questions", {Columns=[
                                         "answerIdx"  , {Type=T_Upto (Const 7)              ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};                                                                                      
                  "Responses", 
                                    {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Training"  , {Type=T_Bool             ; Markup=Input};
                                         "answerIdx" , {Type=T_Upto (Const 7)             ; Markup=Input};
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")), RealConst 1.0))) }; 
                                         "guess"     , {Type=T_Upto (Const 7)             ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};  
                                         "answerIdxP" , {Type=T_Upto (Const 7)            ; Markup=Latent(Gen(StrictIf(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                                  Column("guess"))
                                                                                                                           )) };
                                         ]};
                  ]}


module DifficultyAbilityDiscreteEvaluation =
  let Schema =
      { Name = "DifficultyAbilityDiscreteEvaluation";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]}
                  "Questions", {Columns=[
                                         "answerIdx"  , {Type=T_Upto (Const 7)              ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")),RealConst 1.0))) }; 
                                         "guess"     , {Type=T_Upto (Const 7)             ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};
                                         "answerIdx" , {Type=T_Upto (Const 7)             ; Markup=Observable(Gen(If(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                           Column "guess" )))};                                                                                                                       
                                     ]};                                                                                            
                  "Responses", 
                                    {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Training"  , {Type=T_Bool             ; Markup=Input};
                                         "answerIdx" , {Type=T_Upto (Const 7)              ; Markup=Input};
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")), RealConst 1.0))) }; 
                                         "guess"     , {Type=T_Upto (Const 7)            ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};  
                                         "answerIdxP" , {Type=T_Upto (Const 7)           ; Markup=Latent(Gen(StrictIf(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                                  Column("guess"))
                                                                                                                           )) };
                                         ]};
                  ]}

module DifficultyAbilitySkipDiscreteEvaluation =
  let Schema =
      { Name = "DifficultyAbilitySkipDiscreteEvaluation";
        Tables = ["Students",  {Columns=["ability"   ,    {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))};
                                         "skipPropensity",{Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst -1.3, RealConst 3.3)))};
                                        ]}
                  "Questions", {Columns=[
                                         "answerIdx"  , {Type=T_Upto (Const 7)               ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("studentID",Column "skipPropensity"),RealConst 10.0))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")),RealConst 1.0))) }; 
                                         "guess"     , {Type=T_Upto (Const 7)              ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};
                                         "answerIdx" , {Type=T_Upto (Const 7)             ; Markup=Observable(Gen(If(Column("skipped"),DiscreteConst (0,7),
                                                                                                                  If(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                                    Column "guess" ))))};                                                                                                                       
                                     ]};                                                                                            
                  "Responses", 
                                    {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Training"  , {Type=T_Bool             ; Markup=Input};
                                         "answerIdx" , {Type=T_Upto (Const 7) ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("studentID",Column "skipPropensity"),RealConst 10.0))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")),RealConst 1.0))) }; 
                                         "guess"     , {Type=T_Upto (Const 7)   ; Markup=Latent(Gen(DiscreteUniform(Const(7))))};
                                         "answerIdxP" , {Type=T_Upto (Const 7)  ; Markup=Latent(Gen(If(Column("skipped"),DiscreteConst (0,7),
                                                                                                           If(Column("know"), Deref("questionID",Column "answerIdx"), 
                                                                                                                              Column "guess" ))))};                                                                                                                       
                                         ]};
                  ]}

module DifficultyAbilitySkipGuessDiscreteEvaluation =
  let Schema =
      { Name = "DifficultyAbilitySkipGuess";
        Tables = ["Students",  {Columns=["ability"   ,    {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))};
                                         "skipPropensity",{Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst -1.3, RealConst 3.3)))};
                                        ]}
                  "Questions", {Columns=[
                                         "answerIdx"  , {Type=T_Upto (Const 7)              ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("studentID",Column "skipPropensity"),RealConst 10.0))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")),RealConst 1.0))) }; 
                                         "guess"     , {Type=T_Upto (Const 7)              ; Markup=Latent(CDiscreteWith(Const 7))}; 
                                         "answerIdx" , {Type=T_Upto (Const 7)              ; Markup=Observable(Gen(If(Column("skipped"),
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
                                         "answerIdx" , {Type=T_Upto (Const 7)              ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("studentID",Column "skipPropensity"),RealConst 10.0))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")),RealConst 1.0))) }; 
                                         "guess"     , {Type=T_Upto (Const 7)              ; Markup=Latent(WithPrior(CDiscreteWith(Const 7),Param("ResponsesTrain","guess")))};
                                          "answerIdxP" , {Type=T_Upto (Const 7)            ; Markup=Latent(Gen(If(Column("skipped"),DiscreteConst (0,7),
                                                                                                       If(Column("know"), 
                                                                                                             Deref("questionID",Column "answerIdx"), 
                                                                                                              Column "guess" ))))};                                                                                                                       
                                         ]};
                  ]}

module DifficultyAbilitySkipGuessCheatDiscreteEvaluation =
  let Schema =
      { Name = "DifficultyAbilitySkipGuessCheat";
        Tables = ["Students",  {Columns=["ability"   ,    {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))};
                                         "skipPropensity",{Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst -1.3, RealConst 3.3)))};
                                         "cheats"   , {Type=T_Bool                   ; Markup=Latent(Gen(Bernoulli(RealConst 0.5)))} //TODO check prior
                                        ]}
                  "Questions", {Columns=[
                                         "answerIdx"  , {Type=T_Upto (Const 7)              ; Markup=Input};
                                         "difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};         
                                        ]};
                  "ResponsesTrain", {Columns=
                                        ["studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                         "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("studentID",Column "skipPropensity"),RealConst 10.0))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")),RealConst 1.0))) }; 
                                         "guess"     , {Type=T_Upto (Const 7)            ; Markup=Latent(CDiscreteWith(Const 7))}; //
                                         "answerIdx" , {Type=T_Upto (Const 7)            ; Markup=Observable(Gen(If(Column("skipped"),
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
                                         "answerIdx" , {Type=T_Upto (Const 7)              ; Markup=Input};
                                         "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("studentID",Column "skipPropensity"),RealConst 10.0))) }; 
                                         "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"), Deref("questionID",Column "difficulty")),RealConst 1.0))) }; 
                                         "guess"     , {Type=T_Upto (Const 7)              ; Markup=Latent(WithPrior(CDiscreteWith(Const 7),Param("ResponsesTrain","guess")))};
                                         "answerIdxP" , {Type=T_Upto (Const 7)            ; Markup=Latent(Gen(If(Column("skipped"),DiscreteConst (0,7),
                                                                                                       If(Or(Deref("studentID",Column "cheats"),Column("know")), 
                                                                                                             Deref("questionID",Column "answerIdx"), 
                                                                                                              Column "guess" ))))};                                                                                                                       
                                         ]};
                  ]}
(*
module DifficultyAbility =
  open Tabular
  let Schema =
      {Name = "DifficultyAbility";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                       ; Markup=Latent(CGaussian)} ]}
                  "Answers",   {Columns=["text"      , {Type=T_String                     ; Markup=Input}; ]};
                  "Questions", {Columns=["text"      , {Type=T_String                     ; Markup=Input};
                                          "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer2"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer3"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer4"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer5"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answers"   , {Type=T_Array(T_Link "Answers")   ; Markup=Latent(Gen(Arr
                                                                                                               [| Const 0; //NB: this had better agree with DB ID 1
                                                                                                                  Column "answer0";
                                                                                                                  Column "answer1";
                                                                                                                  Column "answer2";
                                                                                                                  Column "answer3"; 
                                                                                                                  Column "answer4"; 
                                                                                                                  Column "answer5";
                                                                                                               |]))};
                                          "difficulty", {Type=T_Real                      ; Markup=Latent(CGaussian)};         
                                        ]};
                  "Responses", {Columns=[ "studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen( Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty"))))) };
                                          "guess"     , {Type=T_Int              ; Markup=Latent(Gen(DiscreteUniform(Const 7)))};
                                          //NB: need to use StrictIf to avoid AllZeroException bug encountered with If - not sure why
                                          (*
                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(StrictIf(Column("know"), Deref("questionID",Column "answer"),
                                                                                                                                  Index(Deref("questionID",Column "answers"),
                                                                                                                                        Column("guess")))
                                                                                                          )) }; 
                                          *)

                                          "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(//Column("answer"), 
                                                                                                            StrictIf(Column("know"), 
                                                                                                                     Deref("questionID",Column "answer"),
                                                                                                                     Index(Deref("questionID",Column "answers"),
                                                                                                                           Column("guess"))),
                                                                                                            Deref("questionID",Column "answer"))))  }; 
                                          ]} ]}

*)

module DifficultyAbilitySkip =
  open Tabular
  let Schema =
      {Name = "DifficultyAbilitySkip";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                      ; Markup=Latent(CGaussian)}
                                         "skipPropensity"   , {Type=T_Real                      ; Markup=Latent(CGaussian)} // new
                                        ]}
                  "Answers",   {Columns=["text"      , {Type=T_String           ; Markup=Input}; ]};
                  "Questions", {Columns=["text"      , {Type=T_String                    ; Markup=Input};
                                          "answer"    , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer0"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer1"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer2"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer3"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer4"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answer5"   , {Type=T_Link "Answers"            ; Markup=Input};
                                          "answers"   , {Type=T_Array(T_Link "Answers", Const 7)   
                                                                                          ; Markup=Latent(Gen(Arr
                                                                                                               [| Const 0; //NB: this had better agree with DB ID 1
                                                                                                                  Column "answer0";
                                                                                                                  Column "answer1";
                                                                                                                  Column "answer2";
                                                                                                                  Column "answer3"; 
                                                                                                                  Column "answer4"; 
                                                                                                                  Column "answer5";
                                                                                                               |]))};
                                          "difficulty", {Type=T_Real                       ; Markup=Latent(CGaussian)};         
                                        ]};
                  "Responses", {Columns=[ "studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                          "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("studentID",Column "skipPropensity"), RealConst 10.0))) };
                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty")),RealConst 1.0))) };
                                          "guess"     , {Type=T_Upto (Const 7)   ; Markup=Latent(Gen(DiscreteUniform(Const 7)))};
                                          //NB: need to use StrictIf to avoid AllZeroException bug encountered with If - not sure why
                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(StrictIf(Column("skipped"),
                                                                                                                  (*Link("Answers")*) 
                                                                                                                  (Const 0),
                                                                                                                  // Deref("questionID",Column "answer0"), // ppt "Link("Answers") 0" seems wrong
                                                                                                                  StrictIf(Column("know"), 
                                                                                                                           Deref("questionID",Column "answer"),
                                                                                                                           Index(Deref("questionID",Column "answers"),
                                                                                                                                Column("guess")))
                                                                                                          )
                                                                                                          )) }; 
                                          
                                          "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer"))))  }; 
                                          ]} ]}


module DifficultyAbilitySkipCheat =
  open Tabular
  let Schema =
      {Name = "DifficultyAbilitySkipCheat";
        Tables = ["Students",  {Columns=["ability"   , {Type=T_Real                      ; Markup=Latent(CGaussian)}
                                         "skipPropensity", {Type=T_Real                  ; Markup=Latent(CGaussian)} // new
                                         "cheats"   , {Type=T_Bool                    ; Markup=Latent(CBernoulli)} 
                                        ]}
                  "Answers",   {Columns=["text"      , {Type=T_String                    ; Markup=Input}; ]};
                  "Questions", {Columns=["text"      , {Type=T_String                    ; Markup=Input};
                                          "answer"    , {Type=T_Link "Answers"           ; Markup=Input};
                                          "answer0"   , {Type=T_Link "Answers"           ; Markup=Input};
                                          "answer1"   , {Type=T_Link "Answers"           ; Markup=Input};
                                          "answer2"   , {Type=T_Link "Answers"           ; Markup=Input};
                                          "answer3"   , {Type=T_Link "Answers"           ; Markup=Input};
                                          "answer4"   , {Type=T_Link "Answers"           ; Markup=Input};
                                          "answer5"   , {Type=T_Link "Answers"           ; Markup=Input};
                                          "answers"   , {Type=T_Array(T_Link "Answers", Const 7)   
                                                                                         ; Markup=Latent(Gen(Arr
                                                                                                               [| Const 0; //NB: this had better agree with DB ID 1
                                                                                                                  Column "answer0";
                                                                                                                  Column "answer1";
                                                                                                                  Column "answer2";
                                                                                                                  Column "answer3"; 
                                                                                                                  Column "answer4"; 
                                                                                                                  Column "answer5";
                                                                                                               |]))};
                                          "difficulty", {Type=T_Real                     ; Markup=Latent(CGaussian)};         
                                        ]};
                  "Responses", {Columns=[ "studentID" , {Type=T_Link "Students"  ; Markup=Input};
                                          "questionID", {Type=T_Link "Questions" ; Markup=Input};
                                          "skipped"   , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("studentID",Column "skipPropensity"), RealConst 10.0))) };
                                          "know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("studentID",Column "ability"),Deref("questionID",Column "difficulty")),RealConst 1.0))) };
                                          "guess"     , {Type=T_Upto (Const 7)   ; Markup=Latent(Gen(DiscreteUniform(Const 7)))};
                                          //NB: need to use StrictIf to avoid AllZeroException bug encountered with If - not sure why
                                          "answer"    , {Type= T_Link "Answers"  ; Markup=Observable(Gen(StrictIf(Column("skipped"),
                                                                                                                  (*Link("Answers")*) 
                                                                                                                  (Const 0),
                                                                                                                  // Deref("questionID",Column "answer0"), // ppt "Link("Answers") 0" seems wrong
                                                                                                                  StrictIf(Or(Deref("studentID",Column "cheats"),Column("know")), 
                                                                                                                           Deref("questionID",Column "answer"),
                                                                                                                            Index(Deref("questionID",Column "answers"),
                                                                                                                                Column("guess")))
                                                                                                          )
                                                                                                          )) }; 
                                          
                                          "correct"   , {Type=T_Bool             ; Markup=Observable(Gen(EQ(Column("answer"), Deref("questionID",Column "answer"))))  }; 
                                          ]} 
                   ]}



module DARE =
 
  let answerRange = 6
  let T_AnswerRange = T_Upto (Const answerRange)
  let Participant =
      {Name = "Participant";
        Tables = ["Participants",  {Columns=["Ability"   , {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]};
                  "Questions", {Columns=["Answer"  , {Type=T_AnswerRange                  ; Markup=Observable(CDiscreteWith(Const answerRange))};
                                         "Train"   , {Type=T_Bool                 ; Markup=Input};     
                                        ]};
                  "Responses", {Columns=
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Train"   , {Type=T_Bool               ; Markup=Input};
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("ParticipantID",Column "Ability"),RealConst 1.0))) };
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(StrictIf(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                                     Column("Guess")))) };
                                                                                                                          
                                        ]}
                  ]}
  let Question =
      {Name = "Question";
        Tables = ["Participants",  {Columns=["Ability"   , {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]};
                  "Questions", {Columns=["Answer"  , {Type=T_AnswerRange                  ; Markup=Observable(CDiscreteWith(Const answerRange))};
                                         "Train"   , {Type=T_Bool                 ; Markup=Input};
                                         "Difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))};      
                                        ]};
                  "Responses", {Columns=
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Train"   , {Type=T_Bool               ; Markup=Input};
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("ParticipantID",Column "Ability"), Deref("QuestionID",Column "Difficulty")),RealConst 1.0))) };
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(StrictIf(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                                       Column("Guess")))) };
                                                                                                                          
                                        ]}
                  ]}
  let DARE =
      {Name = "DARE";
        Tables = ["Participants",  {Columns=["Ability"   , {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]};
                  "Questions", {Columns=["Answer"  , {Type=T_AnswerRange                  ; Markup=Observable(CDiscreteWith(Const answerRange))};
                                         "Train"   , {Type=T_Bool                 ; Markup=Input};
                                         "Difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))}; 
                                         "Discrimination", {Type=T_Real           ; Markup=Latent((Gen(Gamma(RealConst 0., RealConst 0.5))))};    // revise hypers!   
                                        ]};
                  "Responses", {Columns=
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Train"   , {Type=T_Bool                 ; Markup=Input};
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("ParticipantID",Column "Ability"),
                                                                                                                 Deref("QuestionID",Column "Difficulty")),
                                                                                                                 Deref("QuestionID",Column "Discrimination")))) };
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(StrictIf(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                                     Column("Guess")))) };
                                                                                                                          
                                        ]}
                  ]}

module DAREQuery =
  let answerRange = 8
  let T_AnswerRange = T_Upto (Const answerRange)
  let Participant =
      {Name = "DAREQueryParticipant";
        Tables = ["Participants",  
                               {Columns=["Ability"   , {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]};
                  "Questions", {Columns=["Answer"  ,   {Type=T_AnswerRange                 ; Markup=Latent(CDiscreteWith(Const answerRange))} ]};
                  "QuestionsTR", {Columns=["QuestionID", {Type=T_Link "Questions"  ; Markup=Input};
                                           "Answer"  , {Type=T_AnswerRange                 ; Markup=Observable(Gen(Deref("QuestionID",Column "Answer")))};   
                                        ]};
                  "Responses", {Columns=
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input};     
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Deref("ParticipantID",Column "Ability"),
                                                                                                           RealConst 1.0))) };
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Latent(Gen(If(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                       Column("Guess")))) };
                                        ]};
                  "ResponsesTR", {Columns=
                                        ["ResponseID", {Type=T_Link "Responses" ; Markup=Input };
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(Deref("ResponseID",Column "Answer"))) };                                                                             
                                        ]}
                  ]}

  let Question =
      {Name = "DAREQueryQuestion";
        Tables = ["Participants",  {Columns=["Ability"   , {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]};
                  "Questions", {Columns=["Answer"  , {Type=T_AnswerRange                  ; Markup=Latent(CDiscreteWith(Const answerRange))}; 
                                         "Train"   , {Type=T_Bool                 ; Markup=Input};
                                         "Difficulty", {Type=T_Real               ; Markup=Latent((Gen(Gaussian(RealConst 0., RealConst 0.5))))}; //new      
                                        ]};
                  "QuestionsTR", {Columns=["QuestionID", {Type=T_Link "Questions"  ; Markup=Input};
                                           "Answer"  , {Type=T_AnswerRange                 ; Markup=Observable(Gen(Deref("QuestionID",Column "Answer")))};   
                                        ]};
                  "Responses", {Columns= 
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("ParticipantID",Column "Ability"),
                                                                                                                       Deref("QuestionID",Column "Difficulty")), //new
                                                                                                           RealConst 1.0))) };
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Latent(Gen(If(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                       Column("Guess")))) };                                                                                  
                                        ]};
                   "ResponsesTR", {Columns=
                                        ["ResponseID", {Type=T_Link "Responses" ; Markup=Input };
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(Deref("ResponseID",Column "Answer"))) };                                                                             
                                        ]}
                  ]}
  let DARE =
      {Name = "DAREQuery";
        Tables = ["Participants",  {Columns=["Ability"   , {Type=T_Real           ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 1.)))} ]};
                  "Questions", {Columns=["Answer"  , {Type=T_AnswerRange                  ; Markup=Latent(CDiscreteWith(Const answerRange))};
                                         "Train"   , {Type=T_Bool                 ; Markup=Input};
                                         "Difficulty", {Type=T_Real               ; Markup=Latent(Gen(Gaussian(RealConst 0., RealConst 0.5)))}; 
                                         "Discrimination", {Type=T_Real           ; Markup=Latent(Gen(Gamma(RealConst 1.0, RealConst 1.0)))}; // new    
                                        ]};
                  "QuestionsTR", {Columns=["QuestionID", {Type=T_Link "Questions"  ; Markup=Input};
                                           "Answer"  , {Type=T_AnswerRange                 ; Markup=Observable(Gen(Deref("QuestionID",Column "Answer")))};   
                                        ]};
                  "Responses", {Columns=
                                        ["ParticipantID" , {Type=T_Link "Participants"  ; Markup=Input};
                                         "QuestionID", {Type=T_Link "Questions" ; Markup=Input};
                                         "Know"      , {Type=T_Bool             ; Markup=Latent(Gen(Probit(Minus(Deref("ParticipantID",Column "Ability"),
                                                                                                                 Deref("QuestionID",Column "Difficulty")),
                                                                                                                 Deref("QuestionID",Column "Discrimination")))) };//new
                                         "Guess"     , {Type=T_AnswerRange              ; Markup=Latent(Gen(DiscreteUniform(Const answerRange)))};
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Latent(Gen(If(Column("Know"), Deref("QuestionID",Column "Answer"), 
                                                                                                                       Column("Guess")))) };
                                                                                                                          
                                        ]};
                   "ResponsesTR", {Columns=
                                        ["ResponseID", {Type=T_Link "Responses" ; Markup=Input };
                                         "Answer" ,    {Type=T_AnswerRange              ; Markup=Observable(Gen(Deref("ResponseID",Column "Answer"))) };                                                                             
                                        ]}
                  ]}
module LinearRegression = 
 let LR =
      {Name = "LR";
        Tables = ["Graph",  {Columns=["muA"   , {Type=T_Real           ; Markup=Latent(Hyper(RealConst 0.0))};
                                      "muB"   , {Type=T_Real           ; Markup=Latent(Hyper(RealConst 0.0))};
                                      "A"   , {Type=T_Real           ; Markup=Latent(Prior(Gaussian(Column "muA", RealConst 1.)))};
                                      "B"   , {Type=T_Real           ; Markup=Latent(Prior(Gaussian(Column "muB", RealConst 1.)))};
                                      "X"   , {Type=T_Real           ; Markup=Input};
                                      "Z"   , {Type=T_Real           ; Markup=Latent(Gen(Plus(Times(Column "A", Column "X"),Column "B")))}; 
                                      "Y"   , {Type=T_Real           ; Markup=Observable(Gen(Gaussian(Column "Z", RealConst 1.)))};
                                        ]}
                  ]}
 let LRWithError =
      {Name = "LR";
        Tables = ["Graph",  {Columns=["muA"   , {Type=T_Real           ; Markup=Latent(Hyper(RealConst 0.0))};
                                      "muB"   , {Type=T_Real           ; Markup=Latent(Hyper(Gaussian(RealConst 0.0,RealConst 1.0)))};
                                      "A"   , {Type=T_Real           ; Markup=Latent(Prior(Gaussian(Column "muA", RealConst 1.)))};
                                      "B"   , {Type=T_Real           ; Markup=Latent(Prior(Gaussian(Column "muB", RealConst 1.)))};
                                      "X"   , {Type=T_Real           ; Markup=Input};
                                      "Z"   , {Type=T_Real           ; Markup=Latent(Gen(Plus(Times(Column "A", Column "X"),Column "B")))}; 
                                      "Y"   , {Type=T_Real           ; Markup=Observable(Gen(Gaussian(Column "Z", RealConst 1.)))};
                                        ]}
                  ]}
 let LRWithSimpleError =
      {Name = "LR";
        Tables = ["Graph",  {Columns=["muA"   , {Type=T_Real           ; Markup=Latent(Hyper(RealConst 0.0))};
                                      "muB"   , {Type=T_Real           ; Markup=Latent(Hyper(RealConst 0.0))};
                                      "A"   , {Type=T_Real           ; Markup=Latent(Prior(Gaussian(Column "muA", RealConst 1.)))};
                                      "B"   , {Type=T_Real           ; Markup=Latent(Prior(Gaussian(Column "mu", RealConst 1.)))};
                                      "X"   , {Type=T_Real           ; Markup=Input};
                                      "Z"   , {Type=T_Real           ; Markup=Latent(Gen(Plus(Times(Column "A", Column "X"),Column "B")))};
                                      "W"   , {Type=T_Real           ; Markup=Latent(Prior(Gamma(Column "Z", Column "Z")))}
                                      "Y"   , {Type=T_Real           ; Markup=Observable(Gen(Gaussian(Column "Z", Column "W")))};

                                        ]}
                  ]}