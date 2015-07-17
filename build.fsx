// include Fake lib
#r @"packages\FAKE\tools\FakeLib.dll"
#r @"System.Xml.Linq.dll"
open Fake
open Fake.AssemblyInfoFile
open System.IO
open System.Xml
open System.Linq
open System.Xml.Linq
open Fake.ReleaseNotesHelper


// Properties
let relative subdir = Path.Combine(__SOURCE_DIRECTORY__, subdir)
let buildDir = "./build/"


let projectName (doc:XDocument) =  
   doc.Descendants(xname "Project").Descendants(xname "PropertyGroup").Descendants(xname "Name") |> Seq.tryFind (fun _ -> true)

let projectName2 (doc:XDocument) =  
   let r = projectName doc
   if r.IsSome 
   then r.Value.Value 
   else let an = doc.Descendants(xname "Project").Descendants(xname "PropertyGroup").Descendants(xname "AssemblyName") |> Seq.tryFind (fun _ -> true)
        an.Value.Value

let closure s = 
   getProjectReferences (relative s) |> Set.add s



let project = "Tabular" 
let summary = "Bayesian estimation library"

let description = """             
  Tabular allows you to write bayesian models and infer parameters"""

//let license   = failwith "na"
let tags      = "F# fsharp formatting markdown code fssnip literate programming"
let company   = "Microsoft"
let copyright = "Copyright © Microsoft Research Limited 2014"
let release = ReleaseNotesHelper.parseReleaseNotes (File.ReadLines (relative "TaskPaneDNA/RELEASE_NOTES.md"))

// Targets
Target "Clean" (fun _ ->
    CleanDir buildDir
)

Target "AssemblyFile" (fun _ -> 
   let abs = Set.fold  Set.union  Set.empty //([[1;2];[3]] |> List.map Set.ofList |> Set.ofList)
   let a = [ "TaskPaneDNA/TaskPaneDNA.fsproj" ] |> List.fold (fun s e -> Set.union s (closure e)) Set.empty
   printfn "starting" 
   a
   |> Seq.iter(fun s -> try
                           printfn "loading project %A" s
                           let proj = loadProject s
                           printfn "project %A" (projectName2 proj)
                           if s.EndsWith("fsproj") then
                             let fileName = Path.Combine(Path.GetDirectoryName(s),"AssemblyInfo.fs")
                             CreateFSharpAssemblyInfo fileName   
                                    [ Attribute.Title project
                                      Attribute.Product project
                                      Attribute.Description summary
                                      Attribute.Version release.AssemblyVersion
                                      Attribute.FileVersion release.AssemblyVersion]
                           else
                             let fileName = Path.Combine(Path.GetDirectoryName(s),"AssemblyInfo.cs")
                             CreateCSharpAssemblyInfo fileName   
                                    [ Attribute.Title project
                                      Attribute.Product project
                                      Attribute.Description summary
                                      Attribute.Version release.AssemblyVersion
                                      Attribute.FileVersion release.AssemblyVersion ] 
                        with |exn -> printfn "%A" exn.Message              
                                    )
)

// Default target
Target "Default" (fun _ ->
   ()
   //we can build : build id (relative "MarkupApp.sln")
   //and make nuget etc here...
)

// Dependencies
"Clean"
  ==> "AssemblyFile"
  ==> "Default"

// start build
RunTargetOrDefault "Default"