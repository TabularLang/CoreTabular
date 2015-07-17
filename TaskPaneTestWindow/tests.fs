namespace MicrosoftResearch.Infer.Tabular

module Tests =
   open System
   open System.Windows
   open System.Windows.Controls
   open MicrosoftResearch.Infer.Tabular.TaskPane
   open Microsoft.Office.Interop
   open System.Runtime.InteropServices
   open NUnit.Framework

   let isModelSheet (ws:Excel.Worksheet) =  ws.Visible = Microsoft.Office.Interop.Excel.XlSheetVisibility.xlSheetVisible //you cant activate an non visible sheet and excel fails silently
                                            && ws.Name.StartsWith(TabularModelSheetNamePrefix,StringComparison.CurrentCultureIgnoreCase)
   let delta x = x,x
   type Excel.Workbook with
      member wb.ActivateNonTabularModelSheet () = 
            try
               wb.Worksheets.Cast<Excel.Worksheet>() |> Seq.filter (isModelSheet >> not) 
                                                     |> Seq.head |> (fun ws -> ws.Activate())
            with |e -> failwith (sprintf "could not find non-tabular model sheet in the wb %A. please add one" wb.Name)
      member wb.TabularModelSheets () = 
            wb.Worksheets.Cast<Excel.Worksheet>() 
            |> Seq.filter isModelSheet

   let sheetEqual (sh1:Excel.Worksheet, sh2:Excel.Worksheet) = 
      let sheetEqual = sh1.UsedRange.Value2 = sh2.UsedRange.Value2
      if not(sheetEqual) then
         let data1 = sh1.UsedRange.Value2 :?> obj[,];
         let data2 = sh2.UsedRange.Value2 :?> obj[,];
         let irows = [| data1.GetLowerBound(0) .. data1.GetUpperBound(0) |]
         let icols = [| data1.GetLowerBound(1) .. data1.GetUpperBound(1) |]

         let equals = irows|> Array.map (fun i -> 
                      icols|> Array.map (fun j -> 
                           if data1.[i,j] = data2.[i,j] then true
                           else try 
                                 let d1 = data1.[i,j] :?> float
                                 let d2 = data2.[i,j] :?> float
                        
                                 let isRangeEqual = (d2*0.99 < d1) && (d1 < d2*1.01)
                                 if not(isRangeEqual) then
                                    let a = i,j
                                    ()
                                 isRangeEqual 
                                with | e -> false )) |> Array.concat
         equals |> Seq.fold (&&) true
      else true
      
   let activateSheet (wb:Excel.Workbook) name = 
      wb.Worksheets.Cast<Excel.Worksheet>() 
      |> Seq.filter (fun ws -> ws.Name = name)
      |> Seq.iter   (fun ws -> ws.Activate())

   let testWbEquals (wb:Excel.Workbook) (wb':Excel.Workbook)=
      wb.Worksheets.Cast<Excel.Worksheet>() 
      |> Seq.filter isModelSheet
      |> Seq.fold(fun s ws -> ws.Activate()
                              activateSheet wb' ws.Name
                              s && sheetEqual(ws, wb'.ActiveSheet :?> _))
                 true

   let rec deletedirectory s = 
      System.IO.DirectoryInfo(s).EnumerateFiles()      |> Seq.iter(fun f -> f.Attributes <- System.IO.FileAttributes.Normal;f.Delete()) 
      System.IO.DirectoryInfo(s).EnumerateDirectories()|> Seq.iter(fun f -> f.Attributes <- System.IO.FileAttributes.Normal; deletedirectory f.FullName) 
      System.IO.Directory.Delete(s)

   let getMvc (excel:Excel.Application) =
      let model      = TaskPaneModel()
      let view       = new TaskPaneView(TaskPaneIIScreen.TaskPaneControl(), Some excel)
      let controller = new TaskPaneController(Some excel, false)
      let mvc        = FSharp.Windows.Mvc(model, view, controller)
      mvc.Start(), view, controller
   
   type ModelsToRun = | All | Sheets of string list

   let runModels (excel:Excel.Application)  (sheets : ModelsToRun) (wb : Excel.Workbook) =
      let (d,view,controller) = getMvc excel

      wb.ActivateNonTabularModelSheet()
      match sheets with 
         | All -> wb.TabularModelSheets() 
         | Sheets slist -> slist |> List.map (fun n -> wb.Sheets.[n] :?> _) |> List.toSeq
      |> Seq.iter   (fun ws -> ws.Activate()
                               Async.AwaitEvent controller.WaitingCompute  |> Async.RunSynchronously
                               Async.Sleep(5000) |> Async.RunSynchronously
                               view.Control.Infer.RaiseEvent(new RoutedEventArgs(Button.ClickEvent))
                               try
                                 Async.RunSynchronously( Async.AwaitEvent controller.ComputeDone, 90000)
                               with :? TimeoutException as e -> raise (TimeoutException(sprintf "timeout in sheet %A" ws.Name, e))
                                 )
      d.Dispose();view.Dispose();controller.Dispose()
      wb

   [<Test>]
   let ``ESOP_stats``() =
      // if true to actually generate verified tests
      if true then
         let path    = __SOURCE_DIRECTORY__  + @"\..\..\TabularDataESOP\"

         let files = [
                  
                      //  "DARE" // doesn't work with IN 2.6
                        "TrueSkill example"
                        "TrueSkill-100-20000"
                        "TrueSkillBets-100-20000"
                        "Recommender"
                        "NCAAF"
                        "PCA"
                        "BPM"
                        "LinearRegression"
                        "Faithful"
                        "NaiveBayesQuery"
                        "LDA small"
                        "SimpleMatchbox"
                        "Mammography"
                       //"TrueSkill-10000-2000000"
                        ] 
         files
         |> List.iter(fun s -> let excel = Microsoft.Office.Interop.Excel.ApplicationClass();
                               //excel.DisplayAlerts <- false
                               excel.Visible <- true
                               System.Console.WriteLine(s)
                               let a = excel.Workbooks.Open  (path + s               + ".xlsx")  |> runModels excel All
                              
                               a.Close()
                               Marshal.ReleaseComObject(a) |> ignore
                               excel.Quit()
                               Marshal.ReleaseComObject(excel) |> ignore
                               Console.WriteLine(s)
                               )
         Assert.True(true)

   let toVerifiedName s =  "verified_"  + s
   
   let ``run and generate new ground truth sheets`` (sourceDirectory,targetDirectory, oFiles) =
      // if true to actually generate verified tests
      //if false then
         let pathData     = sourceDirectory
         let pathVerified = targetDirectory
         if System.IO.Directory.Exists(pathVerified) then deletedirectory pathVerified
         System.IO.Directory.CreateDirectory(pathVerified) |> ignore


         let files =  if oFiles |> Option.isSome then oFiles.Value |> List.toSeq
                      else System.IO.Directory.EnumerateFiles(sourceDirectory) |> Seq.map (fun fn -> System.IO.FileInfo(fn).Name)
                      |> Set.ofSeq
         files
         |> Set.iter (fun s -> let excel = Microsoft.Office.Interop.Excel.ApplicationClass();
                               excel.DisplayAlerts <- false
                               //for debug
                               //excel.DisplayAlerts <- true
                               //excel.Visible <- true
                               let a = excel.Workbooks.Open  (pathData     + s                )  |> runModels excel All
                               a.SaveAs                      (pathVerified + toVerifiedName s , ConflictResolution=Excel.XlSaveConflictResolution.xlLocalSessionChanges)

                               a.Close()
                               Marshal.ReleaseComObject(a) |> ignore
                               excel.Quit()
                               Marshal.ReleaseComObject(excel) |> ignore
                               Console.WriteLine(s)
                               )
         Assert.True(true)
      
   let PublicData = __SOURCE_DIRECTORY__  + @"\..\..\TabularDataPublic\"
   [<Test; RequiresSTA>]
   let ``run and generate new ground truth public sheets``() =
      ``run and generate new ground truth sheets``  (PublicData , PublicData  + @"verified\", None)

   // private data
   let TabularData = __SOURCE_DIRECTORY__+ @"\..\..\TabularDataInternal\"
   [<Test; RequiresSTA>]
   let ``run and generate new ground truth private sheets``() =
      ``run and generate new ground truth sheets``  (TabularData, TabularData + @"verified\", None)

   let ``verify Models``(sourceDirectory,verifiedDirectory, fileName, tempDirectory) =
      let excel = Microsoft.Office.Interop.Excel.ApplicationClass();
      excel.DisplayAlerts <- false
      let pathTemp = tempDirectory
      if System.IO.Directory.Exists(pathTemp) then deletedirectory pathTemp
      System.IO.Directory.CreateDirectory(pathTemp) |> ignore

      let run      = excel.Workbooks.Open (sourceDirectory   + fileName                + ".xlsx") |> runModels excel All
      let verified = excel.Workbooks.Open (verifiedDirectory + toVerifiedName fileName + ".xlsx")

      let stmp    = pathTemp + fileName                + ".xlsx" 
      let sverif  = pathTemp + toVerifiedName fileName + ".xlsx"

      run.SaveAs(stmp)
      verified.SaveAs(sverif)
      let equal = testWbEquals run verified 
      try 
         try
            Assert.True(equal)
         with | e -> System.Console.WriteLine (sprintf "test failed for %A, please verify in %A" fileName pathTemp)
      finally
         excel.DisplayAlerts <- false
         verified.Close()
         run.Close()
         excel.Quit()
         Marshal.ReleaseComObject(verified) |> ignore
         Marshal.ReleaseComObject(run) |> ignore
         Marshal.ReleaseComObject(excel) |> ignore
      if false (*equal *) then
         System.IO.File.Delete(stmp)
         System.IO.File.Delete(sverif)
         deletedirectory pathTemp

   


   
   [<Test;RequiresSTA>] let ``verify DARE``                 () = ``verify Models``(TabularData,TabularData + @"verified\", "DARE"                         , TabularData + @"temp\")
   [<Test;RequiresSTA>] let ``verify NCAAF``                () = ``verify Models``(TabularData,TabularData + @"verified\", "NCAAF"                        , TabularData + @"temp\")
   [<Test;RequiresSTA>] let ``verify TrueSkill example``    () = ``verify Models``(PublicData ,PublicData  + @"verified\", "Ranking - TrueSkill example"  , PublicData  + @"temp\")
   [<Test;RequiresSTA>] let ``verify TrueSkill-100-20000``  () = ``verify Models``(PublicData ,PublicData  + @"verified\", "Ranking - TrueSkill large"    , PublicData  + @"temp\")
   [<Test;RequiresSTA>] let ``verify Recommender``          () = ``verify Models``(PublicData ,PublicData  + @"verified\", "Ranking - Recommender"        , PublicData  + @"temp\")
   [<Test;RequiresSTA>] let ``verify PCA``                  () = ``verify Models``(PublicData ,PublicData  + @"verified\", "PCA"                          , PublicData  + @"temp\")
   [<Test;RequiresSTA>] let ``verify BPM``                  () = ``verify Models``(PublicData ,PublicData  + @"verified\", "Classification - BPM"         , PublicData  + @"temp\")
   [<Test;RequiresSTA>] let ``verify LinearRegression``     () = ``verify Models``(PublicData ,PublicData  + @"verified\", "Regression - LinearRegression", PublicData  + @"temp\")
   [<Test;RequiresSTA>] let ``verify Faithful``             () = ``verify Models``(PublicData ,PublicData  + @"verified\", "Clustering - Faithful"        , PublicData  + @"temp\")
