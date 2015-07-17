// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I @".\bin\debug"  //This points to a directory which will have, after compilation, all the dependant dlls in it

#r "DataLayerModel.dll"
#r "Parsing.exe"
#r "Tabular.dll"
#r "TabularModelLibrary.dll"



#r "System.dll"
#r "System.Core.dll"
#r "System.Data.dll"
#r "System.Numerics.dll"


open  MicrosoftResearch.Infer.Tabular
open Syntax


let a = Parsing.ParseMarkup("Latent(GaussianFromMeanAndPrecision(0.0,1.0))")
let b = Parsing.ParseMarkup("Latent(DiscreteUniform(answerRange))")

let c = Parsing.ParseMarkup("Input")
let d = Parsing.ParseMarkup("Latent(0.0)")
let dd = Parsing.ParseMarkup("Output(0.0)")
let e = Parsing.ParseMarkup("Input")
let f = Parsing.ParseMarkup("Input")
let g = Parsing.ParseMarkup("Latent(DampBackward(ParticipantID.Ability - QuestionID.Difficulty,0.2))")
let i = Parsing.ParseMarkup("Latent(DiscreteUniform(answerRange))")
let j = Parsing.ParseMarkup("Latent(if Know then QuestionID.Answer else Guess))")


// Define your library scripting code here

let dac = NewModels.DAREConcrete()


let DAConcrete () = 
   let answerRange = 8
   let T_AnswerRange   = T_Upto (Const (IntConst answerRange))
   [  Table("Participants"   , ["Ability"      , {Type=T_Real             ; Markup=Parsing.ParseMarkup("Latent(GaussianFromMeanAndPrecision(0.0,1.0))")}])
      Table("Questions"    , ["Answer"       , {Type=T_AnswerRange      ; Markup=Parsing.ParseMarkup("Latent(DiscreteUniform(answerRange))")};
                              "Difficulty"   , {Type=T_Real                ; Markup=Parsing.ParseMarkup("Latent(GaussianFromMeanAndPrecision(0.0,1.0))")};
                           //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Input")};
                           //"Training"     , {Type=T_Bool                ; Markup=Parsing.ParseMarkup("Input")};
                             ])                                          
      Table("QuestionsTrain", [ "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Parsing.ParseMarkup("Input")};
                                "Answer"       , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Output(QuestionID.Answer)") }
                              ]);                                         
      Table("Responses",      [  "ParticipantID", {Type=T_Link "Participants" ; Markup=Parsing.ParseMarkup("Input")};
                                 "QuestionID"   , {Type=T_Link "Questions"    ; Markup=Parsing.ParseMarkup("Input")}; 
                                 //"Training"     , {Type=T_Bool                ; Markup=Parsing.ParseMarkup("Input")}; 
                                 "Advantage"    , {Type=T_Real                ; Markup=Parsing.ParseMarkup("Latent(DampBackward(ParticipantID.Ability - QuestionID.Difficulty,0.2))") };
                                 //"Know"         , {Type=T_Bool                ; Markup=Parsing.ParseMarkup("Latent(Probit(Advantage,1.0)") };
                                 "Guess"        , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Latent(DiscreteUniform(answerRange))")};
                                 "Answer"       , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Latent(if Know then QuestionID.Answer else Guess))")};
                                 //"TrueAnswer"   , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Input")};
                              ])
      Table("ResponsesTrain", [  "ResponseID"   , {Type=T_Link "Responses"    ; Markup=Parsing.ParseMarkup("Input") };
                                 "Answer"       , {Type=T_AnswerRange         ; Markup=Parsing.ParseMarkup("Output(ResponseID.Answer)")};
                              ])
   ] 


