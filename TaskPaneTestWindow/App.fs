module MainApp

open System
open System.Windows
open System.Windows.Controls
//open FSharpx
open MicrosoftResearch.Infer.Tabular.TaskPane
open Microsoft.Office.Interop

let loadWindow() =
   let excel = Microsoft.Office.Interop.Excel.ApplicationClass();
   let wb = excel.Workbooks.Open(__SOURCE_DIRECTORY__  + @"\..\TaskPane\data\TrueSkill example.xlsx")
   excel.Visible <- true
   let model = TaskPaneModel()
   let view = new TaskPaneView(TaskPaneIIScreen.TaskPaneControl(), Some (excel :> _))
   let controller = new TaskPaneController(Some (excel :> _), true)
   let mvc = FSharp.Windows.Mvc(model, view, controller)
   mvc.Start() |> ignore

   let window = Window(Title = "wrapper instead of excel", Content = view.Control, Width = 800., Height = 800.)
   window.DataContext <- model
   window   


   

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore