#r @"System.IO.Compression.dll"
#r @"System.IO.Compression.FileSystem.dll"
//#r @".\bin\debug\Interop.IWshRuntimeLibrary.dll"

open System
open System.IO
open System.IO.Compression


let rec deletedirectory s = 
      DirectoryInfo(s).EnumerateFiles() |> Seq.iter(fun f -> f.Attributes <- FileAttributes.Normal;f.Delete()) 
      DirectoryInfo(s).EnumerateDirectories()|> Seq.iter(fun f -> f.Attributes <- FileAttributes.Normal; deletedirectory f.FullName) 
      Directory.Delete(s)

let s = Path.GetTempPath() + "\postbuildUDF32"
//let s = __SOURCE_DIRECTORY__ +  @"\..\TabularTaskPaneDemoTmp"
if Directory.Exists(s) then deletedirectory s


let finalBinDirName  = @"\.binariesTabularUDF\"

let dZip = Directory.CreateDirectory(s)
let dPrgZip = Directory.CreateDirectory(s + finalBinDirName)

let dSource = DirectoryInfo(__SOURCE_DIRECTORY__ + "/bin/debug") 
do dSource.EnumerateFiles() |> Seq.iter(fun f -> f.CopyTo(dPrgZip.FullName + @"\" + f.Name, false) |> ignore)
FileInfo(__SOURCE_DIRECTORY__ + @"\Readme.txt").CopyTo(dZip.FullName + @"\README.txt")
FileInfo(__SOURCE_DIRECTORY__ + @"\install.bat"  ).CopyTo(dZip.FullName + @"\install.bat")
FileInfo(__SOURCE_DIRECTORY__ + @"\uninstall.bat").CopyTo(dZip.FullName + @"\uninstall.bat")


let zipfilename = __SOURCE_DIRECTORY__ + @"\..\TabularTaskpaneVSTOUDF32.zip"
let rec nextfimename zipfilename n = 
    let fi = FileInfo(zipfilename)
    let testname = Path.GetDirectoryName(zipfilename) +  @"\" +  Path.GetFileNameWithoutExtension(zipfilename)  + (if n = 0 then "" else sprintf "(%A)" n) + Path.GetExtension(zipfilename)
    if File.Exists(testname) then
        nextfimename zipfilename (n + 1)
    else 
      testname

if File.Exists(zipfilename) then File.Delete(zipfilename)
ZipFile.CreateFromDirectory(dZip.FullName, zipfilename);


