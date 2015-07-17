// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open NUnit.Framework
open MicrosoftResearch.Infer.Tabular.CLI

let publicData = __SOURCE_DIRECTORY__  + @"\..\..\TabularDataPublic"


[<Test>]
let ``Generate reference data``() =    
    let runTest modelFileName =
        runCLI publicData None modelFileName "." false None None (Reuse "verified") true true true true

    runTest "Clustering - Multivariate Faithful.csv"
    Assert.True(true)


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
