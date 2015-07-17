// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


open MicrosoftResearch.Infer.Tabular

[<EntryPoint>]
let main argv = 
   let _ = Compiler.testTrueSkillWithConjugate 10 20
      
   let _ = Compiler.testTrueSkillWithHyperAndParam 10 20
      
      
   0 // return an integer exit code
