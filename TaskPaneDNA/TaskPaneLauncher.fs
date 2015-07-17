namespace MicrosoftResearch.Infer.Tabular.TaskPane

open System
open System.Threading
open System.Collections
open System.Linq
open System.Collections.Generic
open System.Windows.Forms.Integration
open ExcelDna.Integration.CustomUI
open MicrosoftResearch.Infer.Tabular.DataLayer
open System.Runtime.InteropServices
open Microsoft.Office.Interop.Excel

open System.IO

//some excelDNA logic
type public MyUserControlDNA ()=
   inherit MyUserControl(if (isDesignTime.Value) then (GetWorkbook(__SOURCE_DIRECTORY__ + @"mooc.xlsx").Application |> Some)
                         else try ExcelDna.Integration.ExcelDnaUtil.Application :?> _  |> Some  with | e -> None)


module CTPManager =
    let ctpDic =  new Dictionary<_,CustomTaskPane*IDisposable> ()
    let excel = if (isDesignTime.Value) then (GetWorkbook(__SOURCE_DIRECTORY__ + @"mooc.xlsx").Application |> Some)
                else try ExcelDna.Integration.ExcelDnaUtil.Application :?> _  |> Some  with | e -> None

    let initCTP () = 
         let ctp = if excel.IsSome 
                   then CustomTaskPaneFactory.CreateCustomTaskPane(typeof<MyUserControlDNA>, "Tabular", excel.Value.ActiveWindow)
                   else CustomTaskPaneFactory.CreateCustomTaskPane(typeof<MyUserControlDNA>, "Tabular") 
         ctp.Width <- 350; ctp.Visible <- true; ctp

    let destroyCTP (ctp:CustomTaskPane) =  ctp.Visible <- false;
                                           (ctp.ContentControl :?> IDisposable).Dispose()
                                           ctp.Delete(); 
                                           

    let destroyCTP2 (ctp:CustomTaskPane, listener : IDisposable) =  destroyCTP ctp; listener.Dispose()

    let ToggleCTP(visible:bool) =
      if excel.Value.ActiveWorkbook <> null then
        let wb = excel.Value.ActiveWorkbook
        if visible then
            let ctp = initCTP()
            let listener = wb.ObsClose |> Observable.subscribe(fun f -> destroyCTP ctp)
            ctpDic.Add(wb, (ctp,listener))

        elif ctpDic.ContainsKey(wb) then destroyCTP2 ctpDic.[wb]; ctpDic.Remove(wb) |> ignore

    let dispose () = 
      for k in ctpDic.Keys do destroyCTP2 ctpDic.[k]
      ctpDic.Clear()

[<ComVisible(true)>]
type public MyRibbon() =
    inherit ExcelRibbon()
    member x.OnToggleCTP(control:IRibbonControl, isPressed:bool) = 
      CTPManager.ToggleCTP(isPressed)

    override  x.OnDisconnection(mode,a) =
           base.OnDisconnection(mode,&a)
           GC.Collect();
           GC.WaitForPendingFinalizers();
           GC.Collect();
           GC.WaitForPendingFinalizers();
           CTPManager.dispose()

