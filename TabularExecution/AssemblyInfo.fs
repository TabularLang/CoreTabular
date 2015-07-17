namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Tabular")>]
[<assembly: AssemblyProductAttribute("Tabular")>]
[<assembly: AssemblyDescriptionAttribute("Bayesian estimation library")>]
[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
