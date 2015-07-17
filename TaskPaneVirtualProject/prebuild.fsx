#r @"System.IO.Compression.dll"
#r @"System.IO.Compression.FileSystem.dll"
//#r @".\bin\debug\Interop.IWshRuntimeLibrary.dll"

open System
open System.IO
open System.IO.Compression

System.Console.WriteLine("prebuild.fsx ...")

let rec deletedirectory s = 
      DirectoryInfo(s).EnumerateFiles() |> Seq.iter(fun f -> f.Attributes <- FileAttributes.Normal;f.Delete()) 
      DirectoryInfo(s).EnumerateDirectories()|> Seq.iter(fun f -> f.Attributes <- FileAttributes.Normal; deletedirectory f.FullName) 
      Directory.Delete(s)

let VERSION = "2.6"
let dSource = DirectoryInfo(__SOURCE_DIRECTORY__ + "/../../Dependencies/infernet/" + VERSION) 
let sTarget =               __SOURCE_DIRECTORY__ + "/../../Dependencies/infernet/current"
try 
   if Directory.Exists(sTarget) then deletedirectory sTarget 
   let dTarget = Directory.CreateDirectory(sTarget)

   do dSource.EnumerateFiles() |> Seq.iter(fun f -> printfn "%A" f.Name;f.CopyTo(dTarget.FullName + @"\" + f.Name, false) |> ignore)
with |e -> printfn "could not ensure version of infer.net %A" e

System.Console.WriteLine("...prebuild.fsx done")