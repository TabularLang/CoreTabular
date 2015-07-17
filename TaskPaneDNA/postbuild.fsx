#r @"System.IO.Compression.dll"
#r @"System.IO.Compression.FileSystem.dll"
//#r @".\bin\debug\Interop.IWshRuntimeLibrary.dll"

open System
open System.IO
open System.IO.Compression

System.Console.WriteLine("postbuild.fsx ...")
// set to create a distribution with a packed xll (avoiding all those binaries in bin)
let PACKED = true

let rec deletedirectory s = 
      DirectoryInfo(s).EnumerateFiles() |> Seq.iter(fun f -> f.Attributes <- FileAttributes.Normal;f.Delete()) 
      DirectoryInfo(s).EnumerateDirectories()|> Seq.iter(fun f -> f.Attributes <- FileAttributes.Normal; deletedirectory f.FullName) 
      Directory.Delete(s)

let rec nextfimename zipfilename n = 
    let fi = FileInfo(zipfilename)
    let testname = Path.GetDirectoryName(zipfilename) +  @"\" +  Path.GetFileNameWithoutExtension(zipfilename)  + (if n = 0 then "" else sprintf "(%A)" n) + Path.GetExtension(zipfilename)
    if File.Exists(testname) then
        nextfimename zipfilename (n + 1)
    else 
      testname


let today = System.DateTime.Today
let stamp = sprintf "%04i-%02i-%02i" today.Year today.Month today.Day


let build name (files:FileInfo seq) = 
      let zipfilename = __SOURCE_DIRECTORY__ + @"\..\" + name + "-" + stamp + ".zip"
      let finalBinDirName  = @"\bin\"
      let finalDataDirName = @"\Tabular Examples\"
      let s = Path.GetTempPath() + "\postbuild"
      if Directory.Exists(s) then deletedirectory s
      let dZip     = Directory.CreateDirectory(s)
      let dPrgZip  = Directory.CreateDirectory(s + finalBinDirName)
      let dDataZip = Directory.CreateDirectory(s + finalDataDirName )

      do if not PACKED
         then
            // include all bin files and unpacked xll
            let dSource = DirectoryInfo(__SOURCE_DIRECTORY__ + "/bin/debug") 
            do dSource.EnumerateFiles() |> Seq.iter(fun f -> f.CopyTo(dPrgZip.FullName + @"\" + f.Name, false) |> ignore)
         else
            // include only Infer.Runtime.dll and the packed xlls
            // TODO: sign the xll https://groups.google.com/forum/#!topic/exceldna/LSeqPWCdhPs
            FileInfo(__SOURCE_DIRECTORY__ + @"\bin\Debug\Infer.Runtime.dll").CopyTo(dZip.FullName + @"\bin\Infer.Runtime.dll") |> ignore
            FileInfo(__SOURCE_DIRECTORY__ + @"\bin\Debug\Tabular-32-packed.xll").CopyTo(dZip.FullName + @"\bin\TabularTaskPaneDNA-AddIn32.xll") |> ignore
            FileInfo(__SOURCE_DIRECTORY__ + @"\bin\Debug\Tabular-64-packed.xll").CopyTo(dZip.FullName + @"\bin\TabularTaskPaneDNA-AddIn64.xll") |> ignore
      

      System.IO.File.WriteAllText(dZip.FullName+ @"\Version.txt",stamp)
      FileInfo(__SOURCE_DIRECTORY__ + @"\..\TaskPane\Readme.txt").CopyTo(dZip.FullName + @"\README.txt") |> ignore
      FileInfo(__SOURCE_DIRECTORY__ + @"\..\TaskPane\Getting Started.docx").CopyTo(dZip.FullName + @"\Getting Started.docx") |> ignore
      FileInfo(__SOURCE_DIRECTORY__ + @"\..\TaskPane\Model overview.docx").CopyTo(dZip.FullName + @"\Model overview.docx") |> ignore
      FileInfo(__SOURCE_DIRECTORY__ + @"\..\TaskPane\MSR-LA Software - Tabular.rtf").CopyTo(dZip.FullName + @"\MSR-LA Software - Tabular.rtf") |> ignore
      FileInfo(__SOURCE_DIRECTORY__ + @"\Tabular 32.bat").CopyTo(dZip.FullName + @"\Tabular 32.bat") |> ignore
      FileInfo(__SOURCE_DIRECTORY__ + @"\Tabular 64.bat").CopyTo(dZip.FullName + @"\Tabular 64.bat") |> ignore
      //we should not need config files
      //FileInfo(__SOURCE_DIRECTORY__ + @"\bin\Debug\TabularTaskPaneDNA-AddIn32.xll.config").CopyTo(dZip.FullName + @"\bin\TabularTaskPaneDNA-AddIn32.xll.config", true) |> ignore
      //FileInfo(__SOURCE_DIRECTORY__ + @"\bin\Debug\TabularTaskPaneDNA-AddIn64.xll.config").CopyTo(dZip.FullName + @"\bin\TabularTaskPaneDNA-AddIn64.xll.config", true) |> ignore
      

      files  |> Seq.iter(fun f -> f.CopyTo(dDataZip.FullName  + f.Name, false) |> ignore)


      if File.Exists(zipfilename) then File.Delete(zipfilename)
      ZipFile.CreateFromDirectory(dZip.FullName, zipfilename);

//external release
let  filesIfExist relDirPath = if Directory.Exists(__SOURCE_DIRECTORY__ + relDirPath)  
                               then DirectoryInfo(__SOURCE_DIRECTORY__ + relDirPath ).EnumerateFiles()
                               else Seq.empty

let publicFiles = filesIfExist "/../../TabularDataPublic/"
build "Tabular"  publicFiles

let internalReleaseFiles  = Seq.concat [publicFiles; filesIfExist "/../../TabularDataInternal/"]

build "TabularInternal"  internalReleaseFiles  

System.Console.WriteLine("...postbuild.fsx done")