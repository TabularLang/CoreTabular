namespace MicrosoftResearch.Infer.Tabular
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


module CLI = 
   open System
   open Printf
   open System
   open System.Windows
   open System.Windows.Controls
   open System.Collections.Generic
   open MicrosoftResearch.Infer.Tabular.TaskPane
   open Microsoft.Office.Interop
   open System.Text.RegularExpressions
   open System.Runtime.InteropServices

   let TryGetValue (d : IDictionary<_,_>) key =
      match d.TryGetValue(key) with
      | (true, v) -> Some(v)
      | (false, _) -> None

   let isModelSheet (ws:Excel.Worksheet) =  ws.Name.StartsWith(TabularModelSheetNamePrefix,StringComparison.CurrentCultureIgnoreCase)

   let pad n (msg:string) = 
      let trunc = String(msg |> Seq.truncate n  |> Seq.toArray)
      trunc.PadRight(n)

                   

   type Excel.Workbook with
      member wb.ActivateNonTabularModelSheet () = 
            wb.Worksheets.Cast<Excel.Worksheet>() |> Seq.filter (isModelSheet >> not) 
                                                   |> Seq.head |> (fun ws -> ws.Activate())
      member wb.TabularModelSheets () = 
            wb.Worksheets.Cast<Excel.Worksheet>() 
            |> Seq.filter isModelSheet

   let getMvc (excel:Excel.Application) =
      let model      = TaskPaneModel()
      let view       = new TaskPaneView(TaskPaneIIScreen.TaskPaneControl(), Some excel)
      let controller = new TaskPaneController(Some excel, false)
      let mvc        = FSharp.Windows.Mvc(model, view, controller)
      mvc.Start(), view, controller
   
   type ModelsToRun = | All | Sheets of string list


   let writeStd (verbose:bool) (writer:IO.TextWriter) (msg:string) = 
      if verbose then 
         let tmp = Console.Out;
         Console.SetOut(writer);
         System.Console.WriteLine(msg)
         Console.SetOut(tmp);
      else 
         ()


   let runModels (excel:Excel.Application)(sheets:ModelsToRun)(wb : Excel.Workbook) (writer : string -> string -> unit)=
      let (d,view,controller) = getMvc excel
      wb.ActivateNonTabularModelSheet()
      Async.Sleep(1000) |> Async.RunSynchronously
      let sheets = match sheets with 
                   | All -> wb.TabularModelSheets() 
                   | Sheets slist -> slist |> List.map (fun n -> wb.Sheets.[n] :?> _) |> List.toSeq 
                   |> Seq.toArray
      writer "will treat sheets" (sprintf "%A" (sheets |> Array.map(fun s -> s.Name)))

      sheets
      |> Seq.iter   (fun ws -> writer "activating" ws.Name
                               ws.Activate()
                               Async.AwaitEvent controller.WaitingCompute |> Async.RunSynchronously
                               Async.Sleep(2000) |> Async.RunSynchronously
                               writer "infering" ws.Name
                               view.Control.Infer.RaiseEvent(new RoutedEventArgs(Button.ClickEvent))
                               writer "inference done" ws.Name
                               Async.AwaitEvent controller.ComputeDone  |> Async.RunSynchronously)
      d.Dispose();view.Dispose();controller.Dispose()
      wb
   
   open ParserCLI

   [<STAThread>]
   [<EntryPoint>]
   let main argv = 
      let realConsole = Console.Out;
      let fileCommand = "file"
      let sheetCommand = "sheets"
      let outputCommand = "output"
      let verboseCommand = "verbose"


      //System.Console.WriteLine(printf "%A" argv)

      let defs = [
            {ArgInfo.Command=fileCommand  ; Description="Workbook to run tabular on"                      ; Required=true  }
            {ArgInfo.Command=sheetCommand ; Description="sheets containing models on which to run tabular"; Required=false }
            {ArgInfo.Command=outputCommand; Description="output name"                                     ; Required=false } 
            {ArgInfo.Command=verboseCommand    ; Description="verbose mode"                               ; Required=false } ]
      try
//         let input         = String.Join(" ", argv )
         let parsedArgs    = ParserCLI.parseArgs defs argv
         let wbpath        = parsedArgs.[fileCommand].Head
         let sheets        = match TryGetValue parsedArgs sheetCommand  with | Some(s) -> Sheets s | _ -> All
         let savesAsName   = match TryGetValue parsedArgs outputCommand with | Some(s) -> s.Head   | _ -> wbpath
         let verbose       = TryGetValue parsedArgs verboseCommand |> Option.isSome

         let writer left right = writeStd verbose realConsole (sprintf "%s:  %A" (pad 20 left) right)
         writer "parsedArgs"   parsedArgs
         writer "wbpath"       wbpath
         writer "sheets"       sheets
         writer  "savesAsName"  savesAsName

         try 
            let excel = Microsoft.Office.Interop.Excel.ApplicationClass();
            //excel.Visible <- true
            excel.DisplayAlerts <- false

            let fullpath = 
                 if IO.Path.IsPathRooted(wbpath) then
                     id
                 else 
                     (+) System.AppDomain.CurrentDomain.BaseDirectory

            if true then
               let wb = runModels excel sheets (excel.Workbooks.Open (fullpath  wbpath)) writer
               try 
                  writer "saving file"  savesAsName
                  wb.SaveAs(fullpath savesAsName)
                  writer "file saved" savesAsName
               with | e -> 
                  Console.SetOut(realConsole);
                  System.Console.WriteLine(e.Message)
                  System.Console.WriteLine(sprintf "pb saving the file %A. make sure it is not read only" savesAsName)
   
            //System.Console.ReadLine() |> ignore
               wb.Close()
               Marshal.ReleaseComObject(wb)    |> ignore
            excel.Quit()
            Marshal.ReleaseComObject(excel) |> ignore
          with | e -> 
                      Console.SetOut(realConsole);
                      System.Console.WriteLine(e.Message)
      with |e ->
         Console.SetOut(realConsole);
         System.Console.WriteLine(ParserCLI.printUsage defs)
      0

