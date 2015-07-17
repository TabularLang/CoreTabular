namespace BugTaskPaneFS


open System
//open System.Collections
open System.Linq
open System.Collections.Generic
open System.Windows.Forms.Integration
open Microsoft.Office.Interop.Excel
open System.Windows.Controls
open ExcelDna.Integration.CustomUI
open ExcelDna.Integration
open System.Runtime.InteropServices;


#if THREADBUG

type Agent<'T> = MailboxProcessor<'T>

//some excelDNA logic
type public MyUserControl() as this =
    inherit System.Windows.Forms.UserControl()

    let excel = ExcelDna.Integration.ExcelDnaUtil.Application :?> _  

   //create a simple WPF Control with a button wich reads data model
   //add it to children
   //then add another button to activate ohter button
    let wpfElementHost = new ElementHost(Dock = System.Windows.Forms.DockStyle.Fill)
    let but1 = new Button() |> (fun b -> b.Content <- "click me"; b)
    let but2 = new Button() |> (fun b -> b.Content <- "load"(*; b.IsEnabled <- false*); b)

#if DIRECT
    let s1 = but1.Click |> Observable.subscribe(fun e ->  but2.IsEnabled <- true)
    let s2 = but2.Click |> Observable.subscribe(fun e ->  let app = ExcelDnaUtil.Application :?> Application;
                                                          let ts = app.ActiveWorkbook.Model.ModelTables;
                                                          let tables = ts.Cast<obj>().ToArray();
                                                          but1.Content <- "clicked" ) 
#endif
#if ASYNC
    let rec initial = async {
         let! c = Async.AwaitEvent  but1.Click
         but2.IsEnabled <- true
         but1.IsEnabled <- false
         return! enabled
      }
    and enabled = async {
         let! c = Async.AwaitEvent  but2.Click
         let app = ExcelDnaUtil.Application :?> Application;
         let ts = app.ActiveWorkbook.Model.ModelTables;
         let tables = ts.Cast<obj>().ToArray();

         but1.IsEnabled <- true
         but2.IsEnabled <- false
         return! initial
    }

    do Async.StartImmediate initial
#endif
#if AGENT
    let cts = new System.Threading.CancellationTokenSource()
    let sc = System.Threading.SynchronizationContext.Current
   // do cts.Cancel()
    //do cts.Cancel()
    let startAutoWf () = Agent<unit>.Start(
                              (fun agt -> 
                                     let rec initial() = async {
                                         // do! Async.SwitchToContext(sc)
                                          let! c = Async.AwaitEvent  but1.Click
                                          //but2.IsEnabled <- true
                                          //but1.IsEnabled <- false
                                          if cts.Token.IsCancellationRequested
                                          then ()
                                          else return! enabled()
                                       }
                                     and enabled() = async {
                                          let! c = Async.AwaitEvent  but2.Click
                                          do! Async.SwitchToContext(sc)
                                          let app = ExcelDnaUtil.Application :?> Application;
                                          let ts = app.ActiveWorkbook.Model.ModelTables;
                                          let tables = ts.Cast<obj>().ToArray();

                                          //but1.IsEnabled <- true
                                          //but2.IsEnabled <- false
                                          if cts.Token.IsCancellationRequested
                                          then ()
                                          else return! initial()
                                     }
                                     initial()),cts.Token) 

#endif

    do 
       let m = startAutoWf ()
       let sp = new StackPanel()
       sp.Orientation <- Orientation.Vertical
       sp.Children.Add(but1) |> ignore
       sp.Children.Add(but2) |> ignore
       wpfElementHost.HostContainer.Children.Add(sp) |> ignore
       this.Controls.Add(wpfElementHost)

    interface IDisposable with
      member this.Dispose() = 
       //  let wh = cts.Token.WaitHandle
        // do cts.Cancel()
       //  do wh.WaitOne()
         do ()
         //cts.Cancel()
#if DIRECT
         s1.Dispose()
         s2.Dispose()
#endif
      // ()

#endif


#if POWERPIVOTAPI
type public MyUserControl() as this =
    inherit System.Windows.Forms.UserControl()
    let excel = ExcelDna.Integration.ExcelDnaUtil.Application :?> _  

    let wpfElementHost = new ElementHost(Dock = System.Windows.Forms.DockStyle.Fill)
    let but1 = new Button() |> (fun b -> b.Content <- "without MarshalRelease"; b)
    let but2 = new Button() |> (fun b -> b.Content <- "with    MarshalRelease"; b)
    let but3 = new Button() |> (fun b -> b.Content <- "with    MarshalRelease array"; b)


    let s2 = but1.Click |> Observable.subscribe(fun e ->  let app = ExcelDnaUtil.Application :?> Application;
                                                          let _wb = app.ActiveWorkbook
                                                          let tables1 = _wb.Model;
                                                          let tables2 = tables1.ModelTables;
            
                                                          let en1 = tables2.GetEnumerator();
                                                          do en1.MoveNext() |> ignore;
                                                          let first = en1.Current;
                                                          but1.Content <- "without MarshalRelease" + System.DateTime.Now.ToString()
                                                          ()) 

    let s2 = but2.Click |> Observable.subscribe(fun e ->  let app = ExcelDnaUtil.Application :?> Application;
                                                          let _wb = app.ActiveWorkbook
                                                          let tables1 = _wb.Model;
                                                          let tables2 = tables1.ModelTables;
            
                                                          let en1 = tables2.GetEnumerator();
                                                          do en1.MoveNext() |> ignore;
                                                          let first = en1.Current;

                                                          do
                                                            Marshal.ReleaseComObject(first)   |> ignore;
                                                            Marshal.ReleaseComObject(en1) |> ignore;
                                                            Marshal.ReleaseComObject(tables2) |> ignore;
                                                            Marshal.ReleaseComObject(tables1) |> ignore;
                                                            Marshal.ReleaseComObject(_wb) |> ignore;
                                                            Marshal.ReleaseComObject(app) |> ignore;
                                                          but2.Content <- "with    MarshalRelease" + System.DateTime.Now.ToString()
                                                          ) 

    let s3 = but3.Click |> Observable.subscribe(fun e ->  let app = ExcelDnaUtil.Application :?> Application;
                                                          let _wb = app.ActiveWorkbook
                                                          let tables1 = _wb.Model;
                                                          let tables2 = tables1.ModelTables;
            
                                                          let first = tables2.[1];

                                                          do
                                                            Marshal.ReleaseComObject(first)   |> ignore;
                                                            Marshal.ReleaseComObject(tables2) |> ignore;
                                                            Marshal.ReleaseComObject(tables1) |> ignore;
                                                            Marshal.ReleaseComObject(_wb) |> ignore;
                                                            Marshal.ReleaseComObject(app) |> ignore;
                                                          but3.Content <- "with    MarshalRelease array" + System.DateTime.Now.ToString()
                                                          ) 
    do 
       let sp = new StackPanel()
       sp.Orientation <- Orientation.Vertical
       sp.Children.Add(but1) |> ignore
       sp.Children.Add(but2) |> ignore
       sp.Children.Add(but3) |> ignore
       wpfElementHost.HostContainer.Children.Add(sp) |> ignore
       this.Controls.Add(wpfElementHost)

    interface IDisposable with
      member this.Dispose() = 
         s2.Dispose()
#endif


module CTPManager =
    let mutable ctp:CustomTaskPane option = None
    let excel = try ExcelDna.Integration.ExcelDnaUtil.Application :?> Application  |> Some  with | e -> None

    let ToggleCTP(visible:bool) =
        if visible then
            if excel.IsSome then
               ctp <- Some (CustomTaskPaneFactory.CreateCustomTaskPane(typeof<MyUserControl>, "Hello bug", excel.Value.ActiveWindow))
            else
               ctp <- Some (CustomTaskPaneFactory.CreateCustomTaskPane(typeof<MyUserControl>, "Hello bug")) 
            ctp.Value.Width <- 350
            ctp.Value.Visible <- true
        elif ctp.IsSome then ctp.Value.Visible <- visible
    let dispose () = 
      if ctp.IsSome then (ctp.Value.ContentControl :?> IDisposable).Dispose()


                             

type public MyRibbon() =
    inherit ExcelRibbon()
    member x.OnToggleCTP(control:IRibbonControl, isPressed:bool) = 
      CTPManager.ToggleCTP(isPressed)
    member x.OnDisconnection(_,_) = CTPManager.dispose()