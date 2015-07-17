namespace MicrosoftResearch.Infer.Tabular.TaskPane

module Constants =
   // controls whether the validate button generates a confusion matrix or has the old behaviour
   let TABULAR_EXP = System.Environment.GetEnvironmentVariable("TABULAR_EXP") <> null

open System
open System.Threading
open System.Collections
open System.Linq
open System.Collections.Generic
open System.Windows.Data
open System.Windows.Data
open System.Windows.Controls
open System.Windows.Forms.Integration
open Microsoft.Office.Interop.Excel

open FSharp.Windows
open FSharp.Windows.Binding
open FSharp.Windows
//open Microsoft.FSharp.Reflection


open MicrosoftResearch.Infer.Tabular
open MicrosoftResearch.Infer.Tabular.CrossValidation
open MicrosoftResearch.Infer.Tabular.DataLayer
open MicrosoftResearch.Infer.Tabular.DataLayer.ExcelNames
open MicrosoftResearch.Infer.Tabular.DataLayer.NewIO
module NewTabular = Syntax
open NewTabular
open MicrosoftResearch.Infer.Distributions
open MicrosoftResearch.Infer.Tabular.Plates
open System.IO
//open MicrosoftResearch.Infer.Tabular.Tabular
//open QuickGraph.Serialization

open Wizard
open TypeNormIntegration
open TableNorm
open SchemaConstants

//auto                : helper functions 
//TabularFromToSheet  : functions binding excel to the tabular domain
//InteractionEvents   : interaction events
//TaskPaneView        : shim to the actual view which wires up the GUI events to stream the interaction events
//TaskPaneController  : the controller contains the state machine 

//helper functions
[<AutoOpen>]
module auto = 
  open System.Windows
  open System.ComponentModel
  let TabularModelSheetNamePrefix = "Tabular_"
  let namedRangeModelTitle = "model"
  let debugSM = false

  let approxEq (s1:string) (s2:string) = 
      let eq1 (s1:string) (s2:string) = s1.ToLowerInvariant().Equals(s2.ToLowerInvariant()) 
      eq1 s1 s2 || eq1 (s1.Replace(" ", ""))  (s2.Replace(" ", ""))

  let mutate f x = f x; x
  type IEnumerable with member x.Cast<'T> () = x |> Seq.cast<'T>
  let isDesignTime = lazy (DependencyPropertyDescriptor.FromProperty(DesignerProperties.IsInDesignModeProperty, typeof<FrameworkElement>).Metadata.DefaultValue :?> bool)
  type Microsoft.Office.Interop.Excel.Application with
    member x.ObsWorkbookActivate  =  
      let ev = Event<Workbook>()
      x.add_WorkbookActivate(fun t -> let a = t
                                      ev.Trigger(t) )
      //(x :> AppEvents_Event).add_WorkbookActivate(fun t -> ev.Trigger(t) )
      ev.Publish
    member x.ObsWorkbookClose  =  
      let ev = Event<Workbook>()
      x.add_WorkbookBeforeClose(fun t c -> ev.Trigger(t) )
      ev.Publish

  type Microsoft.Office.Interop.Excel.Workbook with
    member x.ObsActivate  =  
      let ev = Event<unit>()
      x.add_Activate(fun t -> let a = t
                              ev.Trigger() )
      ev.Publish

    member x.ObsClose  =  
      let ev = Event<unit>()
      x.add_BeforeClose(fun t -> let a = t
                                 ev.Trigger() )
      ev.Publish

  type MaybeBuilder() =
      member this.Return(x) = Some x
      member this.Bind(m, f) = Option.bind f m
  let maybe =  MaybeBuilder()


  let smLog(msg:string) = if debugSM then System.Console.WriteLine(msg)
  let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    //printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    timer.ElapsedMilliseconds, returnValue

  let aduration f = async {
         let timer = new System.Diagnostics.Stopwatch()
         let! tok = f |> Async.StartChild
         timer.Start()
         let! r = tok
         return r, timer.ElapsedMilliseconds
      }

   
module Seq = 
  let Stitch f (s:seq<'a>) = Seq.zip s (s |> Seq.map f)
module List =
  open System.Windows.Controls
  //open FSharp.Windows
  let ofButtonClicks xs = xs |> List.map(fun(b : Button, value) -> 
                                            try 
                                                b.Click |> FSharp.Windows.Observable.mapTo value
                                            with |e -> eprintfn<_> "%s" (e.ToString()); null)
module NewTrueskillModel =
  open NewTabular
  let Schema =
    [Declaration(Table("Players",  Some (FromColumn "Player")),
      ["Skill",   {Type=makeDet T_Real R; Markup=Latent(MExp(Dist(GaussianFromMeanAndVariance,[Const (RealConst 25.); Const(RealConst 100.)])))};
       "Rank",   {Type=makeDet T_Real Qry; Markup=Latent(MExp(Infer(GaussianFromMeanAndVariance,[],"Mean",Var "Skill")))};
      ]);
     Declaration(Table("Matches",  Some (FromColumn "Match")),
                       ["Player1", {Type=T_Link "Players"; Markup=Input};
                        "Player2", {Type=T_Link "Players"; Markup=Input};
                        "Perf1",   {Type=makeDet T_Real R; Markup=Latent(MExp(Dist(GaussianFromMeanAndVariance,[DeRef(Var "Player1", "Players", "Skill"); Const(RealConst 100.0)])))};
                        "Perf2",   {Type=makeDet T_Real R; Markup=Latent(MExp(Dist(GaussianFromMeanAndVariance,[DeRef(Var "Player2", "Players", "Skill"); Const(RealConst 100.0)])))};
                        "Win1" ,   {Type=makeDet T_Bool R; Markup=Observable(MExp(Prim(Gt, [Var "Perf1"; Var "Perf2"])))} ]) ]

type IdToPos = Map<TableName,Map<int,int>>
type DicData = Map<TableName, Map<ColumnName,int> * (obj array seq)>
type Agent<'T> = MailboxProcessor<'T>
type D<'T> = IDistribution<'T>


//functions binding excel to the tabular domain
[<AutoOpen>]
module TabularFromToSheet =
  let modelNameToSheetName = (+) TabularModelSheetNamePrefix >> (fun s -> s.Substring(0, (min 31 s.Length)))
  //let sizeMap tables = List.fold (fun (map:Map<string,int>) (name, _) -> map.Add(name,50)) (Map.empty) tables //We should instead transform down to a graph and make it 50^incoming edges to be more 'realistic'
  //let posIDtoPosMap (DTO dto) =  dto |> Map.map(fun tablename (_, rows)->Seq.mapi(fun i _->i :> IComparable,i) rows |> Map.ofSeq) (*in our case, the identity *is* the position *)

  let writeNewModelToSheet (m:NewTabular.Schema) (ws:Worksheet) = 
      let res : (string*string*string*string) list = Pretty.toPositional2DStr m
      for c = 1 to 4 do (ws.Cells.[1,c]:?>Range).EntireColumn.ClearContents() |> ignore
      res |> Seq.iteri(fun i (a,b,c,d) ->
                                (ws.Cells.[1+ i, 1] :?> Range).Value2 <- a
                                (ws.Cells.[1+ i, 2] :?> Range).Value2 <- b
                                (ws.Cells.[1+ i, 3] :?> Range).Value2 <- c
                                (ws.Cells.[1+ i, 4] :?> Range).Value2 <- d
                       )
      for c = 1 to 4 do (ws.Cells.[1,c]:?>Range).EntireColumn.AutoFit() |> ignore

  let get2DStringModel (ws:Worksheet) = 
    let rg1 = ws.Range(namedRangeModelTitle)
    let data1 = rg1.Value2 :?> (obj[,])
    let emptyrow i = Array.TrueForAll(data1.[i, 1 ..], fun x -> x = null || x.ToString() = "")
    let lastNonEmptyRow = ref (data1.GetLength 0)
    while emptyrow !lastNonEmptyRow do lastNonEmptyRow := !lastNonEmptyRow - 1
    let toString (o:obj) =  if o = null then "" else  (o.ToString())
    data1.[1 .. (!lastNonEmptyRow + 1), 1 .. 4 ] |> Array2D.map toString


  let parseWorksheetParsingArea (ws:Worksheet) = 
    let data1= get2DStringModel ws 
    let clb = data1.GetLowerBound(1)
    let area= [ for i in (data1.GetLowerBound(0)) .. (data1.GetUpperBound(0)) ->
                     (data1.[i,clb+0],data1.[i,clb+1],data1.[i,clb+2],data1.[i,clb+3],Some i) ]
    let r = SchemaParser.readSchema area
    r
  

[<AutoOpen>]
module InteractionEvents =
  //interaction events for the state machine
  type MainSM = 
    | SheetActivate   of   string
    | TryParseModel 
    | SampleFromPrior
    | ComputePosterior 
    | StopComputingPosterior 
    | SaveGraph

  let (|OnModelSheet|_|) ms = 
      match ms with
      | SheetActivate(name) -> if (name.StartsWith(TabularModelSheetNamePrefix,StringComparison.CurrentCultureIgnoreCase)) 
                               then Some (name.Substring(TabularModelSheetNamePrefix.Length)) else None
      | _           ->  None

  type AutoCompileSM = 
      | ModelChanged 
      | SuccessfulCompile

  //all interaction events
  type TaskPaneEvents = 
    | MainSM of MainSM
    | GraphDirectoryDisplay
    | GraphDirectoryRemove
    | ValidateData
    | AutoCompileSM of AutoCompileSM
    | PrintDefaultModel


 
[<AutoOpen>]
module Globals =
  let sTempGraph = Path.GetTempPath() + "Tabular"

module Observable_ = Microsoft.FSharp.Control.Observable
//shim to the actual view which wires up the GUI events to stream the interaction events
type TaskPaneView(ctrl,excel : Application option) = 
  inherit View<TaskPaneEvents, TaskPaneModel, TaskPaneIIScreen.TaskPaneControl>(ctrl)
 
  //ahhhrg
  let mutable stupidRef  = excel.Value.ActiveWorkbook

  let isInRange (r:Range) = r.Column < 5
  let t = new System.Timers.Timer() |> mutate (fun s -> s.AutoReset <- false)
  let o = excel.Value.SheetChange |> Observable_.filter isInRange |> Observable_.subscribe(fun r -> t.Stop(); t.Start())
  let watcher = new System.IO.FileSystemWatcher(Path.GetTempPath())
  do watcher.EnableRaisingEvents <- true
     watcher.NotifyFilter <- (NotifyFilters.DirectoryName||| NotifyFilters.LastWrite)
  member x.Control = ctrl
  override this.EventStreams = 
      let paneStream = [ yield! [ ctrl.PriorSample , MainSM(SampleFromPrior); 
                                  ctrl.Infer       , MainSM(ComputePosterior)
                                  ctrl.Stop        , MainSM(StopComputingPosterior)
                                  ctrl.save_graph  , MainSM(SaveGraph)
                                  ctrl.ValidateData, ValidateData
                                  ctrl.applyDefaultModel, PrintDefaultModel ] |> List.ofButtonClicks
                         yield  Observable_.merge ctrl.InlineModel.Checked  ctrl.InlineModel.Unchecked |> Observable_.map(fun e -> MainSM(TryParseModel);) ];
      let s = Globals.sTempGraph
      let fileSystem = [ watcher.Renamed |> Observable_.filter (fun d -> d.FullPath = s) |> Observable_.map (fun e -> GraphDirectoryDisplay)
                         watcher.Created |> Observable_.filter (fun d -> d.FullPath = s) |> Observable_.map (fun e -> GraphDirectoryDisplay)
                         watcher.Deleted |> Observable_.filter (fun d -> d.FullPath = s) |> Observable_.map (fun e -> GraphDirectoryRemove) ]
      t.Interval <- 500.
      let excelStream = 
        if excel.Value.ActiveWorkbook <> null then
          [  excel.Value.ActiveWorkbook.ObsActivate |> Observable_.map(fun ev -> let a = ev
                                                                                 MainSM(SheetActivate((excel.Value.ActiveSheet :?> Worksheet).Name)))
             excel.Value.ActiveWorkbook.SheetActivate  |> Observable_.map(fun ev -> let a = ev
                                                                                    MainSM(SheetActivate((excel.Value.ActiveSheet :?> Worksheet).Name)))
             t.Elapsed |> Observable_.map(fun _ -> MainSM(TryParseModel))
             t.Elapsed |> Observable_.map(fun _ -> AutoCompileSM(ModelChanged))
                ]
        else []

      [ yield! paneStream; yield! fileSystem; yield! excelStream]

  override this.SetBindings model = ()
  override x.Dispose() =   stupidRef <- null
                           watcher.EnableRaisingEvents <- false
                           watcher.Dispose()
                           t.Dispose()


//the controller contains the state machine
type TaskPaneController(excel : Application option, openPPForBug: bool) as this = 
    inherit Controller<TaskPaneEvents, TaskPaneModel>()
    let getActiveWb () = excel.Value.ActiveWorkbook

    let evComputeDone = Event<unit>()
    let evWaitingCompute = Event<unit>()

    let addModelLog (m:TaskPaneModel) msg= 
      m.ModelLogMsg <-  (System.String.Format("{0:T}: {1}", DateTime.Now, msg)) :: m.ModelLogMsg |> Seq.truncate 1 |> Seq.toList

    let addRuntimeLog (m:TaskPaneModel) msg= 
      m.RunLogMsg   <-  (System.String.Format("{0:T}: {1}", DateTime.Now, msg)) :: m.RunLogMsg  |> Seq.truncate 4 |> Seq.toList

    let writeConsole  (msg:string)= 
      System.Console.WriteLine(System.String.Format("{0:T}: {1}", DateTime.Now, msg))

    let disableEnableStateEvent state events =
        excel.Value.ScreenUpdating <- false
        excel.Value.Calculation <- XlCalculation.xlCalculationManual
        excel.Value.EnableEvents <- false
        fun () -> 
            excel.Value.EnableEvents <- events;
            excel.Value.Calculation <- state
            excel.Value.ScreenUpdating <- true



    let mutable internalSM = Option<Agent<AutoCompileSM>>.None
    let mutable mainSM = Option<Agent<MainSM>>.None
    let sc = System.Threading.SynchronizationContext.Current
    let cts = new System.Threading.CancellationTokenSource()


    let (|CheckValidSchema|_|) activeModel = 
         match activeModel with 
         | Some(Valid(schema)) ->  do Schema.checkSchema schema
                                   Some schema
         | _ -> None


    let inputColor = 0xD59B5B // blue
    let paramHyperLatentColor = 0xA5A5A5 // gray
    let observableColor = 0x317DED //orange

    let maxModelLength = 400
    let defineNamedRangeModel() =
        let ws = excel.Value.ActiveSheet :?> Worksheet
        //eraseTag ws namedRangeModelTitle // don't do this - deletes cut'n'paste buffer
        let modelRng = ensureTag2D ws ((1,1),(maxModelLength,4)) namedRangeModelTitle 
        ()

     
    let registerFormatters() = 
        let ws = excel.Value.ActiveSheet :?> Worksheet
        let modelRng = ws.Range(namedRangeModelTitle)
        //if modelRng.FormatConditions.Count = 0 then ignore (
        let _ = modelRng.ClearFormats()
        modelRng.NumberFormat <- "@" 
        let CONTAINS kw = "ISNUMBER(SEARCH(\""+kw+"\",OFFSET(A1,0,3-COLUMN())))"
        let OR(s:string[]) = sprintf "OR(%s)" (String.Join(",",s))
        let cns = modelRng.FormatConditions
        let fc1 = cns.Add(XlFormatConditionType.xlExpression,Formula1= (("="+CONTAINS "input"):>obj) ) :?> FormatCondition
        do fc1.Interior.Color <- inputColor
        let fc2 = cns.Add(XlFormatConditionType.xlExpression,Formula1= (("="+CONTAINS "output"):>obj) ) :?> FormatCondition
        do fc2.Interior.Color <- observableColor
        let fc3 = cns.Add(XlFormatConditionType.xlExpression,
                          Formula1= (  "= NOT(ISBLANK(OFFSET(A1,0,3-COLUMN())))"
                                       //("=" + OR[| CONTAINS "hyper"; CONTAINS "param";CONTAINS "local";CONTAINS "latent"|])
                                       :>obj) ) :?> FormatCondition
        do fc3.Interior.Color <- paramHyperLatentColor    
    do Parsing.init()
     
    
    let crossValidate (m:TaskPaneModel)  = 
         match getActiveWb(), m.ActiveModel with 
         | wb, CheckValidSchema(schema) -> 
            try 
                
                m.Computing <- true
                m.ConsoleRedir.Reset()
//                let! (_,t) =  aduration (async { 
//                                    do! Async.SwitchToContext(sc)
                                                                                             
                                    //READ
                                    //use loader1 = new ExcelLoader(wb :?> _, openPPForBug) :> ILoader

                let _,dicLog, dicIdStrategy, dicDatas,schema, _, idToPos = 
                                                                        use loader = new DataModelLoader(wb :?> _, openPPForBug) :> ILoader
                                                                        let (_,_,(typedCoreSchema,_)) = Elaborator.elaborate(schema)
                                                                        readTable loader typedCoreSchema      
 
                //use loader = new DataModelLoader(wb :?> _, openPPForBug) :> ILoader
               // let _,dicLog, dicIdStrategy, dicDatas, _, _, idToPos = readTable loader schema 
//                do! Async.SwitchToThreadPool()
                                          
                // CREATE CONFUSION MATRIX
                let classTableRowData, classcolIdx, foreignTableName = parseDTOForClassifier (DTO(dicDatas)) schema
                /// confusion matrix mean
                let (cmMean, threshold) = doValidationCM_allWays (DTO(dicDatas)) classTableRowData schema classcolIdx
                let name = "CM"
//                    let potentialName = m.ActiveModelName.Value |> modelNameToSheetName |> ((flip (+)) "_CM") // 30 character limit
//                    if System.String.Length potentialName >= 31
//                    then "CM"
                let ws = name |> (ensureSheet wb) 
                ws.Activate() // switch to the CM worksheet

                let state  = excel.Value.Calculation 
                let events = excel.Value.EnableEvents
                excel.Value.ScreenUpdating <- false
                excel.Value.Calculation <- XlCalculation.xlCalculationManual
                excel.Value.EnableEvents <- false
                try 
//                    let logEvidence = ensureTag ws (1,8) "tabular_LogEvidence"
//                    logEvidence.Value2 <- "Log Evidence"
//                    logEvidence.Font.Bold <- true
//                    logEvidence.Offset(1,0).Value2 <- le
                    

                    //WRITE
//                    use storer = if m.SaveToCsv
//                                    then new CSVStorer(ws, schema , m.ActiveModelPosteriorNameToRow) :> IStorer
//                                    else new ExcelStorerNew(ws, schema , m.ActiveModelPosteriorNameToRow) :> IStorer
                                  
//                    let dataTag  
//                    let getFreshTable rangeName (roff1,coff1) (roff2,coff2) = 
//                      let findAndClearNamedTable  pred = ws.ListObjects.Cast<ListObject>() |> Seq.filter(fun (e:ListObject) -> pred(e.Name) )
//                                                         |> Seq.iter (fun rname -> rname.Delete()|> ignore)
//                      findAndClearNamedTable(fun rname -> rname = ws.Name + "!"+ rangeName)
//                      let c = ws.Range(dataTag.Offset(roff1,  coff1), dataTag.Offset(roff2,  coff2))
//                      let t = ws.ListObjects.Add(XlListObjectSourceType.xlSrcRange,c,XlListObjectHasHeaders=XlYesNoGuess.xlYes)
//                      t.Name <-rangeName
                                  
                    // my writing code
                    (*let mytag = ensureTag ws (1,1) "confusion_matrix" //ensureTag2D ws ((1,1), (Array2D.length1 cmMean,Array2D.length2 cmMean)) "confusion_matrix" //storer.getFreshTable "confusion_matrix" (1,1) (2+Array2D.length1 cmMean,2+Array2D.length2 cmMean) //
                    mytag.Offset(2,0).Value2 <- "predicted values"
                    mytag.Offset(0,2).Value2 <- "true values"
                    for i = 0 to (Array2D.length1 cmMean - 1) do
                        mytag.Offset(i+2,1).Value2 <- i // todo: write the labels instead of numbers
                    for j = 0 to (Array2D.length2 cmMean - 1) do
                        mytag.Offset(1,j+2).Value2 <- j
                    for i = 0 to (Array2D.length1 cmMean - 1) do
                        for j = 0 to (Array2D.length2 cmMean - 1) do
                            mytag.Offset(i+2,j+2).Value2 <- cmMean.[i,j]
                    *)

                    // get classifier column labels
                    let foriegnTableVals = getForeignLabels foreignTableName schema idToPos

                    do outputConfusionMatrixAndGraph cmMean ws foriegnTableVals threshold

                                                
                    excel.Value.EnableEvents <- events
                    excel.Value.Calculation <- state
                    excel.Value.ScreenUpdating <- true
                with | e ->
                    excel.Value.EnableEvents <- events
                    excel.Value.Calculation <- state
                    excel.Value.ScreenUpdating <- true
                    raise e
//                do! Async.SwitchToThreadPool()

                m.Computing <- false
                evComputeDone.Trigger() 
//                              })
//                addRuntimeLog m (sprintf "confusion matrix generation for model %A took %.1f s" m.ActiveModelName.Value (float t/1000.))
            with 
                | e -> addRuntimeLog m (e.Message + (if e.InnerException<>null then e.InnerException.Message else "")) ; m.Computing <- false//;   raise e
            

//                                          // replacing with new functionality: do validation
//                                          let classTableRowData, classcolIdx = getAllRowDataAndClassColIdx (DTO(dicDatas)) schema
//                                          /// confusion matrix mean
//                                          let cmMean = doValidationCM_allWays (DTO(dicDatas)) classTableRowData schema classcolIdx
//                                          let ws = m.ActiveModelName.Value |> modelNameToSheetName |> ((flip (+)) "_Confusion") |> (ensureSheet wb) 
//                                          use storer =  if m.SaveToCsv
//                                                        then new CSVStorer(ws, schema , m.ActiveModelPosteriorNameToRow) :> IStorer
//                                                        else new ExcelStorerNew(ws, schema , m.ActiveModelPosteriorNameToRow) :> IStorer
//                                          
//                                          addRuntimeLog m "ok"
         |  _,_ -> ()
                                         
    
    //this wf post compile/sample message to the main loop if the auto mode is enabled
    //we are binding to the external optional reference of SM because of mutual initialization
    let startAutoWf m = Agent<AutoCompileSM>.Start(
                              (fun agt -> 
                                  let rec AwaitingForChange((*sm:Agent<SM>,*)m:TaskPaneModel) = async {
                                      try 
                                         let! ms = agt.Receive()
                                         match ms with
                                         | ModelChanged when m.IsAutoMode ->  return! AwaitingForCompile(m) 
                                         | _                              ->  return! AwaitingForChange(m) 
                                       with | e -> System.Console.WriteLine(e.Message)
                                                   return! AwaitingForChange(m)
                                    } 
                                  and AwaitingForCompile (m) = async {
                                       try 
                                         let! ms = agt.Receive()
                                         match ms with
                                         | SuccessfulCompile  -> if m.IsAutoMode && m.LockMode.IsSome then
                                                                     match m.LockMode.Value with 
                                                                        | a when a = TaskPaneModel.sSample -> mainSM.Value.Post(SampleFromPrior)
                                                                        | a when a = TaskPaneModel.sInfer  -> mainSM.Value.Post(ComputePosterior)
                                                                        | _ -> failwith "impossible" 
                                         | _               ->  ()
                                         return! AwaitingForChange(m)
                                       with | e -> System.Console.WriteLine(e.Message)
                                                   return! AwaitingForChange(m)
                                    }
                                  AwaitingForChange((*sm,*)m)),cts.Token)

    let startSM m = Agent<MainSM>.Start(
                        (fun agt -> 
                            let rec AwaitingForSheet(m:TaskPaneModel) = async {
                                try 
                                   do! Async.SwitchToContext(sc)
                                   m.ModelPerformingMsg <- "Go to a sheet containing a model" 
                                   addModelLog  m "awaiting sheet"
                                   smLog "AwaitingForSheet"
                                   m.ConsoleRedir.Reset()
                                   m.ActiveModel  <- None
                                   let! ms = agt.Receive()
                                   match ms with
                                   | OnModelSheet(name) -> m.ActiveModelName <- Some name
                                                           defineNamedRangeModel()
                                                           registerFormatters()
                                                           writeConsole "Added formatters" 
                                                           return! OnModelSheet(m, true)
                                   | SheetActivate(_)   ->  return! AwaitingForSheet(m)
                                   | _                  -> return! AwaitingForSheet(m) //() // failwithf "invalid message : %A for state %A" ms "awaiting"
                                 with | e -> System.Console.WriteLine(e.Message)
                                             return! AwaitingForSheet(m)
                              } 
                            and OnModelSheet (m:TaskPaneModel, skipone) = async {
                                    m.ActiveModel <- None
                                    m.ModelPerformingMsg <- "Detected model - compiling" 
                                    smLog "OnModelSheet" 
                                    m.ConsoleRedir.Reset()
                                    addModelLog  m (sprintf "compiling model %A" m.ActiveModelName.Value)
                                    agt.Post(TryParseModel);
                                    return! CompileLoop(m,skipone) 
                               }
                            and CompileLoop (m:TaskPaneModel, skipone) = async {
                                 try
                                    m.ActiveModel  <-  None
                                    smLog "CompileLoop" 
                                    let! ms = agt.Receive()
                                    match ms with
                                      | SheetActivate(name) -> agt.Post(ms); return! AwaitingForSheet(m)
                                      | TryParseModel     ->    m.ModelPerformingMsg <- "Detected model - compiling" 
                                                                let! tok = async {do! Async.SwitchToContext(sc)
                                                                                  return parseWorksheetParsingArea (excel.Value.ActiveSheet :?> _) } |> Async.StartChild
                                                                let! res = tok
                                                                let (error, model, posToRow, settings)  = res
                                                                // Dylan: do we use a and s?
                                                                //let a = Pretty.schemaToStr model.Value
                                                                //let s = model.Value.ToString()
                                                                do! Async.SwitchToThreadPool()
                                                                if settings.IsSome then
                                                                  let oAlgo = settings.Value.AsEnumerable() |> Seq.tryFind(fun kv -> approxEq algorithmLabel kv.Key) 
                                                                  if oAlgo.IsSome then
                                                                     try
                                                                        let algo = m.AvailableDefaultAlgo |> List.find(fun a -> a.Name = (oAlgo.Value.Value |> string))
                                                                        m.SelectedAlgo <- algo |> Some
                                                                     with |e -> failwith (sprintf "can not find algo '%A' specified in the settings" oAlgo.Value)
                                                                  let oIterations = settings.Value.AsEnumerable() |> Seq.tryFind(fun kv -> approxEq iterationsLabel kv.Key) 
                                                                  if oIterations.IsSome then
                                                                     try  m.NumberOfIterations <- coerce oIterations.Value.Value
                                                                     with |e -> failwith (sprintf "can not convert the setting '%A' of value %A to int" iterationsLabel oIterations.Value.Value)
                                                                  let oSaveInput = settings.Value.AsEnumerable() |> Seq.tryFind(fun kv -> approxEq saveinpuLabel kv.Key) 
                                                                  if oSaveInput.IsSome then
                                                                     try m.SaveInput <- coerce oSaveInput.Value.Value
                                                                     with |e -> failwith (sprintf "can not convert the setting '%A' of value %A to bool" saveinpuLabel oSaveInput.Value.Value)                                                                  
                                                                m.ModelPerformingMsg <- "" 
                                                                m.ModelReportingMsg <- error
                                                                m.ActiveModelPosteriorNameToRow <- posToRow
                                                                m.ActiveModel <- model |> Option.map ((fun model -> if m.InlineModel then 
                                                                                                                       Syntax.coreS (Library.prelude @ model)
                                                                                                                    else model) >> Valid)
                                                                match m.ActiveModel with 
                                                                | CheckValidSchema(schema) ->  addModelLog  m (sprintf "successful compile of model %A" m.ActiveModelName.Value)
                                                                                               return! ManualLoop(m, None) 
                                                                | _                        ->  m.ModelPerformingMsg <- "Failed parsing - please correct the model" 
                                                                                               addModelLog  m (sprintf "awaiting correction for model %A" m.ActiveModelName.Value)
                                                                                               return! CompileLoop(m, skipone) 
                                      | _                 -> failwith "invalid message : %A for state %A" ms "OnModelSheet"
                                 with | e -> m.ModelReportingMsg <- e.Message
                                             System.Console.WriteLine(e.Message)
                                             return! CompileLoop (m, false) 
                              }
                            and ManualLoop (m:TaskPaneModel, oCtsLong:System.Threading.CancellationTokenSource option) = async {
                                 try
                                    do! Async.SwitchToContext(sc)
                                    Console.SetOut(m.ConsoleRedir)
                                    Console.SetError(m.ConsoleRedir)

                                    m.ModelPerformingMsg <- "Your model is verified and ready to use !" 
                                    internalSM.Value.Post(SuccessfulCompile)

                                    evWaitingCompute.Trigger()
                                    smLog "ManualLoop" 
                                    let! ms = agt.Receive()
                                    do! Async.SwitchToContext(sc)
                                    m.ModelPerformingMsg <- ""
                                    match ms with
                                      | SheetActivate(name) -> agt.Post(ms);return! AwaitingForSheet(m)
                                      | TryParseModel     -> return! OnModelSheet(m, false)
                                      | StopComputingPosterior ->   if oCtsLong.IsSome then oCtsLong.Value.Cancel() 
                                                                                            addRuntimeLog m "cancel inference requested. this can take a while"
                                                                    return! ManualLoop(m, None)
                                      | SampleFromPrior
                                      | ComputePosterior -> let ctsLong = new System.Threading.CancellationTokenSource()
                                                            m.Computing <- true
                                                            m.ConsoleRedir.Reset()
                                                            let task  =  aduration (async { 
                                                               do! Async.SwitchToContext(sc)
                                                               match  getActiveWb(), m.ActiveModel with
                                                               | null, _                   -> ()
                                                               | wb, Some(Valid(schema))   ->
                                                                     let modelName  = match m.ActiveModelName with None -> "model" | Some s -> s
                                                                     let _, _, dicIdStrategy, _, schema, db,  _  = 
                                                                        use loader = new DataModelLoader(wb :?> _, openPPForBug) :> ILoader
                                                                        let (_,_,(typedCoreSchema,_)) = Elaborator.elaborate(schema)
                                                                        readTable loader typedCoreSchema      
                                                                     let wbname = wb.Name
                                                                     let codefile = if m.ExtractCode 
                                                                                    then 
                                                                                        System.IO.Path.GetTempPath() + @"Tabular\"+modelName+".cs" 
                                                                                    else null
                                                                     do! Async.SwitchToThreadPool()
                                                                     let (typedCoreSchema,le, odb)  as res =
#if R2
                                                                         (*  match m.SelectedAlgo with 
                                                                           | Some  (:? (R2Compiler.R2Algorithm)) ->
                                                                              (R2Compiler.latentModel(name,schema)).performInferenceGeneric(DTO dicDatas,algo, Some m.NumberOfIterations , Some ctsLong.Token ) |> Async.RunSynchronously
                                                                           | _ ->*)
#endif
#if ESOP
                                                                           //warm up jit for timings
                                                                           let _ = ExcelCompiler.latentModel(wb.Name,name,schema,false,false) .performInferenceGeneric(DTO dicDatas,algo, Some m.NumberOfIterations ) |> Async.RunSynchronously
#endif
                                                                          
                                                                           ExcelCompiler.latentModelStraight(wbname, modelName,
                                                                                                             schema,true,true,codefile, db, 
                                                                                                             (match ms with | SampleFromPrior -> None | _ -> m.SelectedAlgo),
                                                                                                              m.BreakSymmetry,
                                                                                                              m.NumberOfIterations , m.RandomSeed,  ctsLong.Token )
                                                                     do! Async.SwitchToContext(sc)
                                                                     let ws = ensureSheet wb (modelNameToSheetName modelName)
                                                                     let logEvidence = ensureTag ws (1,8) "tabular_LogEvidence"
                                                                     logEvidence.Value2 <- "Log Evidence"
                                                                     logEvidence.Font.Bold <- true
                                                                     logEvidence.Offset(1,0).Value2 <- le
                                                                     if m.ExtractCode then
                                                                       logEvidence.Offset(3,0).Formula <- sprintf "=Hyperlink(\"%s\",\"model code\")" codefile
                                                                     let restore = disableEnableStateEvent excel.Value.Calculation excel.Value.EnableEvents
                                                                     //tmp
                                                                     try  let dir = writeExcel m.SaveToCsv ws typedCoreSchema DistributionPrinter.distToString db odb
                                                                          if m.SaveToCsv then logEvidence.Offset(4,0).Formula <- sprintf "=Hyperlink(\"%s\",\"csv files\")" dir
                                                                                              //save model
                                                                                              TypedDTO.write2DArrayToCSV (System.IO.Path.GetDirectoryName(wb.FullName))
                                                                                                                         (modelName + ".csv") 
                                                                                                                         (get2DStringModel ws |> Array2D.map box)
                                                                                              //save input as well
                                                                                              TypedDTO.writeCSV   (System.IO.Path.GetDirectoryName(wb.FullName))
                                                                                                                  (Schema.onlyTableInputAndObservables typedCoreSchema)
                                                                                                                  DistributionPrinter.distToString
                                                                                                                  db
                                                                     finally restore()
                                                                     do! Async.SwitchToThreadPool()
                                                               | _   , _           -> ()
                                                               m.Computing <- false
                                                               evComputeDone.Trigger() })
                                                            do Async.StartWithContinuations(task, 
                                                                                          (fun  (_,t) -> m.ModelReportingMsg <- "inference done"
                                                                                                         addRuntimeLog m (sprintf "inference for model %A took %.1f s" m.ActiveModelName.Value (float t/1000.))),
                                                                                          (fun e      -> addRuntimeLog m (e.Message + (if e.InnerException<>null then e.InnerException.Message else "")) 
                                                                                                         m.Computing <- false
                                                                                                         m.ModelReportingMsg <- e.Message
                                                                                                         evComputeDone.Trigger()   ), 
                                                                                          (fun t      -> m.Computing <- false
                                                                                                         m.ModelReportingMsg <- "inference canceled"
                                                                                                         evComputeDone.Trigger()), 
                                                                                          ctsLong.Token)
                                                            return! ManualLoop(m, Some ctsLong)
                                      | SaveGraph         -> try 
                                                                  this.saveOnSheet(m)
                                                             with | e -> addRuntimeLog m (e.Message + (if e.InnerException<>null then e.InnerException.Message else "")) ; m.Computing <- false; raise e
                                                             m.Computing <- false
                                                             return! ManualLoop(m, None)
                                 with | e -> m.ModelReportingMsg <- e.Message
                                             addRuntimeLog m (e.Message)
                                             agt.Post(TryParseModel);
                                             return! CompileLoop(m,false) 
                              }
                            AwaitingForSheet(m)),cts.Token)

    override this.Dispatcher = function
          | MainSM(msg)        -> Sync (fun _ -> mainSM.Value.Post(msg))
          | AutoCompileSM(msg) -> Sync (fun _ -> internalSM.Value.Post(msg))
          | PrintDefaultModel  -> Sync (this.PrintDefaultModel)
          | GraphDirectoryDisplay -> Sync (this.DisplayGraphLink true)
          | GraphDirectoryRemove  -> Sync (this.DisplayGraphLink false)
          | ValidateData          -> Sync (this.ValidateData)



    override this.InitModel(model : TaskPaneModel) = 
      model.HasGraphViz <- Option.isSome <| MicrosoftResearch.Infer.Tabular.Plates.getGraphvizLocation()
      model.CheatSheet <- ("Distribution types:" ::
                           "---------------------------------" ::
                           Checker.printDistTypes ()) @ 
                          ("" ::
                           "Builtin functions:" ::  
                           "---------------------------------" ::
                           List.map Pretty.declToStr Library.prelude
                           @
                           "Syntax:" ::  
                           "---------------------------------" ::
                           [Help.help])
      let delay e = fun () -> e
       
      let EXPERIMENTAL = if not(Constants.TABULAR_EXP) 
                         then []
                         else [
                                           {Name = "Wizard Classifier";
                                             getit = (fun () -> 
                                                        let wb = getActiveWb()
                                                        //let rawSchema = use loader = new ExcelLoader(wb,openPPForBug) :> ILoader in loader.GetSchema()
                                                        //generateClassifierSchema rawSchema  // old version
                                                        try
                                                            Some ( (defaultArg (model.ActiveModelName) "") + " Classifier",
                                                                   wizNaiveBayesClassifier wb chooseClassifierTable_sClassifierTable chooseClassifierColumn_first putExtraTables_shadow selectDiscreteColumns_smart)
                                                        with
                                                            |e -> //System.Console.WriteLine(e.Message); None
                                                                addRuntimeLog model e.Message
                                                                None
                                                        )};
                                           {Name="Auto-Normalize Data Model";
                                            getit = (fun () ->
                                                        let wb = getActiveWb()

                                                        let dbgraph = getDBGraph wb
                                                        let origDTO = getDataModelData wb
                                                        let tableDTO = convertTableDTO dbgraph origDTO
                                                        let normDTO = normalize tableDTO

                                                        let newWS = ensureSheet wb "Data_Tabular_norm"

                                                        let newLOs = syncTableDTO newWS normDTO
                                                        
                                                        None // doesn't create a Tabular model
                                                        
                                                        )};
                                            {Name="Cross Validate";
                                            getit = (fun () ->
                                                        crossValidate model
                                                        None // doesn't create a Tabular model
                                                        )}


                             ] 
#if R2  
      if (Constants.TABULAR_EXP) then model.AvailableDefaultAlgo <- model.AvailableDefaultAlgo @ [new R2Compiler.R2Algorithm() :> MicrosoftResearch.Infer.IAlgorithm]
#endif 
      model.AvailableDefaultModels <-  [ (*(("TrueSkill",NewTrueskillModel.Schema)::NewModels.DAREProgression)  |> List.map(fun (n,e) -> {Name=n; getit=(fun () -> e)}) *)
                                         [ {Name ="Default Model" ; 
                                            getit= (fun () ->  
                                                    Some ("Default Model",
                                                          (use loader = new DataModelLoader(getActiveWb(),openPPForBug) :> ILoader
                                                           loader.GetSchema()))
                                                   )} ];
                                         [ {Name ="Typed Model" ; // HERE
                                            getit=(fun () ->
                                                       match model.ActiveModel with
                                                       | Some(Valid(schema)) ->
                                                            let (log,err,(typedFullSchema,_)) = Schema.typeSchema schema
                                                            if err 
                                                            then 
                                                                 addModelLog model (Elaborator.logToString log)
                                                                 None 
                                                            else Some ((defaultArg (model.ActiveModelName) "") + " Typed",
                                                                       typedFullSchema)
                                                        | Some(Error m) ->  
                                                                addModelLog model m
                                                                None
                                                        | None ->  
                                                                addModelLog model "cannot produce Typed Model from invalid schema"
                                                                None
                                                    )} ];
                                          [ {Name ="Core Model" ; // HERE
                                            getit=(fun () ->
                                                       match model.ActiveModel with
                                                       | Some(Valid(schema)) ->
                                                            let (log,err,(coreSchema,_)) = Elaborator.elaborate(schema)
                                                            if err then 
                                                               addModelLog model (Elaborator.logToString log)
                                                               None 
                                                            else Some ((defaultArg (model.ActiveModelName) "") + " Core",
                                                                        coreSchema)
                                                        | Some(Error m) ->  
                                                               addModelLog model m
                                                               None
                                                        | None ->  
                                                               addModelLog model "cannot produce Core Model from invalid schema"
                                                               None
                                                    )} ];
                                        
                                         (("TrueSkill",NewTrueskillModel.Schema)::[])(*NewModels.DAREProgression*) |> List.map(fun (n,e) -> {Name=n; getit= delay (Some (n, e))});
                                         EXPERIMENTAL
                                       ] |> List.concat
     
      internalSM <- startAutoWf model |> Some
      mainSM     <- startSM model |> Some

      model.DisplayGraphLink <- Directory.Exists(sTempGraph) 
      model.GraphLink        <- sTempGraph
      Console.SetOut(model.ConsoleRedir)
      Console.SetError(model.ConsoleRedir)

    member x.ComputeDone =  
      evComputeDone.Publish

    member x.WaitingCompute  =  
      evWaitingCompute .Publish


    /// called when user clicks "Validate input"
    member this.ValidateData  (m:TaskPaneModel) = 
      match  getActiveWb(), m.ActiveModel with
      | null, _                        -> ()
      
      | wb, CheckValidSchema(schema)  -> 
                                          use loader = new DataModelLoader(wb :?> _,openPPForBug) :> ILoader
                                          addRuntimeLog m "starting data validation..."
                                          writeConsole "starting data validation...\n"
                                          let (_,_,(typedCoreSchema,_)) = Elaborator.elaborate(schema)
                                          let r = aduration(async { return validateLinks loader typedCoreSchema })
                                          m.Validating <- true
                                          do Async.StartWithContinuations(r, (fun ((_,dicLog, dicIdStrategy, dicDatas, _, _, idToPos),_) -> 
                                                                                 dicLog |> Map.iter (fun table log -> writeConsole (table+  " : " + log.ToString())) 
                                                                                 addRuntimeLog m "data validation finished, results in outputwindow" 
                                                                                 writeConsole "data validation finished"
                                                                                 m.Validating <- false),
                                                                                 (fun e -> writeConsole e.Message;
                                                                                           m.Validating <- false; 
                                                                                           writeConsole "data validation finished"),
                                                                                 (fun _ -> m.Validating <- false))
      

    member  this.saveOnSheet  (m:TaskPaneModel) =  
      let rec deletedirectory s = 
         DirectoryInfo(s).EnumerateFiles() |> Seq.iter(fun f -> f.Attributes <- FileAttributes.Normal;f.Delete()) 
         DirectoryInfo(s).EnumerateDirectories()|> Seq.iter(fun f -> f.Attributes <- FileAttributes.Normal; deletedirectory f.FullName) 
         Directory.Delete(s)

      match  getActiveWb(), m.ActiveModel with
      | null, _                   -> ()
      | wb, CheckValidSchema(schema)   -> 
                                       if Directory.Exists(sTempGraph) then deletedirectory sTempGraph
                                       Directory.CreateDirectory(sTempGraph) |> ignore
                                       let (log,err,(typedCoreSchema,_)) = Elaborator.elaborate schema
                                       do plates m.ActiveModelName.Value sTempGraph sTempGraph typedCoreSchema
                                       //excel.Value.ScreenUpdating <- false
                                       try
                                          let oAc = if excel.Value.ActiveCell <> null then 
                                                      let c = excel.Value.ActiveCell// :?> Range
                                                      Some (c.Row,c.Column)
                                                    else None
   
                                          let pics = ((wb.ActiveSheet :?> Worksheet).Pictures(Type.Missing) :?> Pictures)
                                          let optlt = (pics.Cast<Picture>()) |> Seq.tryPick(fun pic ->  if (pic.Name = m.ActiveModelName.Value) then
                                                                                                          let l,t = pic.Left, pic.Top
                                                                                                          pic.Delete()  |> ignore
                                                                                                          Some(l,t)
                                                                                                         else
                                                                                                          None )  

                                          let p = 
                                             let name = sTempGraph + @"\" + m.ActiveModelName.Value + ".dot" +  ".gif"
                                             let width, height = 
                                                   use img = new System.Drawing.Bitmap(name)
                                                   img.Width, img.Height

                                             let temp = pics.Insert(name)
                                             temp.Width  <- (float)width
                                             temp.Height <- (float)height

                                             temp.Copy() |> ignore
                                             (excel.Value.ActiveSheet :?> Worksheet).PasteSpecial("PNG")
                                             temp.Delete() |> ignore
                                             excel.Value.Selection :?> Picture

                                          optlt |> Option.iter(fun (l,t)->  p.Left <- l; p.Top <- t) 
                                          p.Name <- m.ActiveModelName.Value
                                          oAc |> Option.iter (fun (r,c) -> ((wb.ActiveSheet :?> Worksheet).Cells.[r,c] :?>Range).Activate() |> ignore)
                                          ()
                                       with |e -> ()
                                       //excel.Value.ScreenUpdating <- true
      | _           -> ()

                                                                 


    member this.PrintDefaultModel  (m:TaskPaneModel) = 
      match getActiveWb(), m.SelectedDefaultModel with
      | wb, Some(schema) -> try  
                              let previousSheet = excel.Value.ActiveSheet
                              match m.SelectedDefaultModel.Value.getit() with
                              | None -> ()
                              | Some (name,schema) -> 
                                  let sheetName = modelNameToSheetName name
                                  let modelWS = ensureSheet wb sheetName
                                  modelWS |> writeNewModelToSheet schema 
                                  (previousSheet :?> Worksheet).Activate()
                                  modelWS.Activate()
                            with e -> writeConsole ("something went wrong :" + e.ToString())
      | _ , _            -> ()



    member this.DisplayGraphLink (display:bool) (m:TaskPaneModel) = 
      m.DisplayGraphLink <- display

    override this.Dispose() = cts.Cancel()


//some excelDNA logic
type public MyUserControl(excel:Microsoft.Office.Interop.Excel.Application option) as this =
    inherit System.Windows.Forms.UserControl()

    ///let wb = if excel.IsSome && excel.Value.ActiveWorkbook <> null then Some excel.Value.ActiveWorkbook  else None
    let model = TaskPaneModel()
    let view =  new TaskPaneView(TaskPaneIIScreen.TaskPaneControl(),excel)
    let controller = new TaskPaneController(excel, true)
    let mvc = FSharp.Windows.Mvc(model, view, controller)

    do
        let wpfElementHost = new ElementHost(Dock = System.Windows.Forms.DockStyle.Fill)
        this.Controls.Add(wpfElementHost)
        wpfElementHost.HostContainer.Children.Add(view.Control) |> ignore
        mvc.Start() |> ignore
        if excel.IsSome && excel.Value.ActiveSheet <> null then
            let previousSheet = excel.Value.ActiveSheet :?> Worksheet
            openPPModel(excel.Value, excel.Value.ActiveWorkbook.Name)
            let rec nextws i = 
                  let ws = (excel.Value.Sheets.[1+i] :?> Worksheet)
                  let n = (ws.Parent :?> Workbook).Sheets.Count
                  if ws.Name = previousSheet.Name && n > 1 then 
                      nextws ((i+1)%n)
                  else ws
            (nextws 0).Activate()
            previousSheet.Activate()
    
    interface IDisposable with
      member this.Dispose() = 
         this.Visible <- false
         view.Dispose()
         controller.Dispose()
         ()
