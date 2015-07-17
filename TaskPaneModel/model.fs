namespace MicrosoftResearch.Infer.Tabular.TaskPane

open System
open System.Threading
open System.Collections
open System.Collections.Generic
open FSharp.Windows
open MicrosoftResearch.Infer.Tabular.Syntax
open MicrosoftResearch.Infer.Tabular


open System.ComponentModel
open System.Windows.Controls
open System.Windows

open FSharp.Windows
open FSharp.Windows.Binding
open MicrosoftResearch.Infer

type IdToPos = Map<TableName,Map<IComparable,int>>
type DicData = Map<TableName, Map<ColumnName,int> * (obj array seq)>
type DelayedModel = {Name:string ; getit: unit -> (string * Schema) option } 
type ModelStatus = | Error of string | Valid of Schema





//----------------------
//This holds the state of the UI
//----------------------
type TaskPaneModel() = 
  inherit FSharp.Windows.Model()
  let isDesignTime = lazy (DependencyPropertyDescriptor.FromProperty(DesignerProperties.IsInDesignModeProperty, typeof<FrameworkElement>).Metadata.DefaultValue :?> bool)
  
  //let mutable _OldActiveModel = None :Tabular.Database<Tabular.Markup> option
  let mutable _ActiveModel = None : ModelStatus option
  let mutable _ActiveModelName = None : string option
  let mutable _ActiveModelPosteriorNameToRow = Map.empty : Map<string,Map<string,int>>
  let mutable _AvailableDefaultModels = []

  let mutable _InlineModel            = false
  let mutable _IsSelectedAlgo         = true
  let mutable _AvailableDefaultAlgo   = [new ExpectationPropagation() :> IAlgorithm
                                         new VariationalMessagePassing():> IAlgorithm
                                         new GibbsSampling():> IAlgorithm]
                                          
  let mutable _SelectedAlgo           = Some (_AvailableDefaultAlgo.[0])

  let mutable _SelectedDefaultModel = None : DelayedModel option
  //let mutable _SampleResult = None: Option<Tabular.Database<Tabular.Markup>  * DTO> 
  let mutable _ModelPerformingMsg  = ""    //pertains to model parsing etc..
  let mutable _ModelLogMsg         = []    //pertains to model parsing etc..   
  let mutable _ModelReportingMsg   = ""    //pertains to model parsing etc..
//  let mutable _RunPerformingMsg    = ""    //runtime etc..
  let mutable _RunLogMsg      = []    //runtime etc..
  let mutable _CheatSheet     = []    //runtime etc..
  let mutable _Computing    = false
  let mutable _Validating   = false
  let mutable _LockMode = None :string option
  let mutable _IsAutoMode = false
  let mutable _SavePosterior = true
  let mutable _BreakSymmetry = true
  let mutable _SaveInput = false
  let mutable _SaveToCsv = false
  let mutable _ExtractCode = false   
  let mutable _NumberOfIterations = 30
  let mutable _RandomSeed = ExcelCompiler.defaultRandomSeed
  let mutable _HasGraphViz  = System.IO.File.Exists(@"C:\Program Files (x86)\Graphviz2.34\bin\dot.exe") 
  let mutable _DisplayGraphLink = true
  let mutable _GraphLink = @"C:\Users\crusso\AppData\Local\Temp\Tabular"
  //let mutable _CodeLink = @"C:\Users\crusso\AppData\Local\Temp\Tabular"
  //let mutable _CsvLink = @"C:\Users\crusoo\AppData\Local\Temp\Tabular"
  

  do if (isDesignTime.Value) then 
      _AvailableDefaultModels <- NewModels.DAREProgression |> List.map(fun (name, model) -> {Name=name; getit=(fun () -> Some (name,model))})
      _ModelPerformingMsg  <- "_ModelPerformingMsg I am running/ready"   
      _ModelLogMsg         <- ["go to a sheet";"two";"three";"four";"five"]
      _ModelReportingMsg   <- "_ModelReportingMsg : type error / parsing error "    
      _RunLogMsg           <- ["time1 : last log";"time2 : log before that";"time3 : log before that";"time : log before that";"time5 : log before that"]
      _CheatSheet          <- ["time1 : last log";"time2 : log before that";"time3 : log before that";"time : log before that";"time5 : log before that"]
      _ActiveModel <- Some(Valid(NewModels.DAREProgression |> List.head  |> snd))
      _HasGraphViz <- false

  static member sSample = "Sample"
  static member sInfer  = "Infer"

  member this.ActiveModel with get() = _ActiveModel and set value = _ActiveModel <- value ; this.NotifyPropertyChanged <@this.ActiveModel@>
  member this.ActiveModelName with get() = _ActiveModelName and set value = _ActiveModelName <- value ; this.NotifyPropertyChanged <@this.ActiveModelName@>
  member this.ActiveModelPosteriorNameToRow with get() = _ActiveModelPosteriorNameToRow and set value = _ActiveModelPosteriorNameToRow<- value; this.NotifyPropertyChanged <@this.ActiveModelPosteriorNameToRow @>

  member val ConsoleRedir = new StringRedir() with get, set 
  member val LockModePossible = [TaskPaneModel.sSample;TaskPaneModel.sInfer]

//abstract OldActiveModel    : Tabular.Database<Tabular.Markup> option with get, set
// default this.OldActiveModel with get() = _OldActiveModel and set value = _OldActiveModel <- value
//member this.OldActiveModel 
//    with get() = Option<_>.None
//                  match this.ActiveModel with 
//                 | Some (Valid(schema)) -> 
//                   try Some (NewToOld.trSchema this.ActiveModelName.Value schema) with e -> raise e 
//                 | _ -> 
//                 None
//    and set value = _OldActiveModel <- value; this.NotifyPropertyChanged <@this.ActiveModel@>
  
  member this.AvailableDefaultModels   with get() = _AvailableDefaultModels   and set value = _AvailableDefaultModels <- value; this.NotifyPropertyChanged <@this.AvailableDefaultModels   @>
  member this.AvailableDefaultAlgo     with get() = _AvailableDefaultAlgo     and set value = _AvailableDefaultAlgo <- value  ; this.NotifyPropertyChanged <@this.AvailableDefaultAlgo     @>
  member this.InlineModel              with get() = _InlineModel              and set value = _InlineModel <- value           ; this.NotifyPropertyChanged <@this.InlineModel              @>
  member this.IsSelectedAlgo           with get() = _IsSelectedAlgo           and set value = _IsSelectedAlgo <- value        ; this.NotifyPropertyChanged <@this.IsSelectedAlgo           @>
  member this.SelectedAlgo             with get() = _SelectedAlgo             and set value = _SelectedAlgo <- value          ; this.NotifyPropertyChanged <@this.SelectedAlgo             @>
  member this.Computing                with get() = _Computing                and set value = _Computing <- value             ; this.NotifyPropertyChanged <@this.Computing                @>
  member this.Validating               with get() = _Validating               and set value = _Validating    <- value         ; this.NotifyPropertyChanged <@this.Validating               @>
  member this.ModelPerformingMsg       with get() = _ModelPerformingMsg       and set value = _ModelPerformingMsg <- value    ; this.NotifyPropertyChanged <@this.ModelPerformingMsg       @>
  member this.ModelLogMsg              with get() = _ModelLogMsg              and set value = _ModelLogMsg <- value           ; this.NotifyPropertyChanged <@this.ModelLogMsg              @>
  member this.ModelReportingMsg        with get() = _ModelReportingMsg        and set value = _ModelReportingMsg <- value     ; this.NotifyPropertyChanged <@this.ModelReportingMsg        @>
  member this.RunLogMsg                with get() = _RunLogMsg                and set value = _RunLogMsg <- value             ; this.NotifyPropertyChanged <@this.RunLogMsg                @>
  member this.CheatSheet               with get() = _CheatSheet               and set value = _CheatSheet <- value           ; this.NotifyPropertyChanged <@this.CheatSheet               @>
  member this.SelectedDefaultModel     with get() = _SelectedDefaultModel     and set value = _SelectedDefaultModel <- value  ; this.NotifyPropertyChanged <@this.SelectedDefaultModel     @>
  member this.LockMode                 with get() = _LockMode                 and set value = _LockMode <- value              ; this.NotifyPropertyChanged <@this.LockMode                 @>
  member this.IsAutoMode               with get() = _IsAutoMode               and set value = _IsAutoMode <- value            ; this.NotifyPropertyChanged <@this.IsAutoMode               @>
  member this.SavePosterior            with get() = _SavePosterior            and set value = _SavePosterior <- value         ; this.NotifyPropertyChanged <@this.SavePosterior            @>
  member this.BreakSymmetry            with get() = _BreakSymmetry             and set value = _BreakSymmetry  <- value       ; this.NotifyPropertyChanged <@this.BreakSymmetry           @>
  member this.SaveToCsv                with get() = _SaveToCsv                and set value = _SaveToCsv   <- value           ; this.NotifyPropertyChanged <@this.SaveToCsv                @>
  member this.SaveInput                with get() = _SaveInput                and set value = _SaveInput   <- value           ; this.NotifyPropertyChanged <@this.SaveInput                @>
  member this.NumberOfIterations       with get() = _NumberOfIterations       and set value = _NumberOfIterations <- value    ; this.NotifyPropertyChanged <@this.NumberOfIterations       @>
  member this.RandomSeed               with get() = _RandomSeed       and set value = _RandomSeed <- value    ; this.NotifyPropertyChanged <@this.RandomSeed       @>
  member this.HasGraphViz              with get() = _HasGraphViz              and set value = _HasGraphViz <- value           ; this.NotifyPropertyChanged <@this.HasGraphViz              @>
  member this.DisplayGraphLink         with get() = _DisplayGraphLink         and set value = _DisplayGraphLink <- value      ; this.NotifyPropertyChanged <@this.DisplayGraphLink         @>
  member this.GraphLink                with get() = _GraphLink                and set value = _GraphLink <- value             ; this.NotifyPropertyChanged <@this.GraphLink                @>
  member this.ExtractCode             with get() = _ExtractCode            and set value = _ExtractCode <- value         ; this.NotifyPropertyChanged <@this.ExtractCode            @>
 // member this.CodeLink                with get() = _CodeLink                and set value = _CodeLink <- value             ; this.NotifyPropertyChanged <@this.CodeLink                @> 
//  member this.CsvLink                with get() = _CsvLink                and set value = _CsvLink <- value             ; this.NotifyPropertyChanged <@this.CsvLink                @> 
type OptionSchemaTypeConverter() =
    interface System.Windows.Data.IValueConverter with
        member x.Convert(value: obj, targetType: Type, parameter: obj, culture: System.Globalization.CultureInfo) =
            match value with
            | null -> "" |> box
            | o -> match o :?> _ with
                    |Some(Valid(a)) -> sprintf "%A" a |> box
                    | _ -> box ""

        member x.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: System.Globalization.CultureInfo) =
            failwith "na" 
