namespace FSharp.Windows


open System
open System.Globalization
open System.Windows.Data
open System.Windows;
open System.Linq;



module Handlers = 
    let Hyperlink_RequestNavigate(sender:obj, e:System.Windows.Navigation.RequestNavigateEventArgs ) = 
         System.Diagnostics.Process.Start(new System.Diagnostics.ProcessStartInfo(e.Uri.AbsoluteUri)) |> ignore
         e.Handled = true;



[<AutoOpen>]
module Converters =
    open System;
    open System.Windows
    open System.Windows.Data

    let nullFunction = fun value target param culture -> value
    let nullMCFunction  = fun value target param culture -> box value 
    let nullMCBFunction = fun value target param culture -> [| value |]
    /// abstract class for converter

    let boolToDisplay b = if b then Visibility.Visible else Visibility.Collapsed

  


    [<AbstractClass>]
    type ConverterBase(convertFunction, convertBackFunction) =
        new() = ConverterBase(nullFunction, nullFunction)
        interface IValueConverter with
            override this.Convert(value, targetType, parameter, culture) =     this.Convert value targetType parameter culture
            override this.ConvertBack(value, targetType, parameter, culture) = this.ConvertBack value targetType parameter culture
        abstract member Convert : (obj -> Type -> obj -> Globalization.CultureInfo -> obj)
        default this.Convert = convertFunction
        abstract member ConvertBack : (obj -> Type -> obj -> Globalization.CultureInfo -> obj)
        default this.ConvertBack = convertBackFunction

    [<AbstractClass>]
    type MultiConverterBase(convertFunction, convertBackFunction) =
        new() = MultiConverterBase(nullMCFunction, nullMCBFunction)
        interface IMultiValueConverter with
            override this.Convert(value, targetType, parameter, culture) =     this.Convert value targetType parameter culture
            override this.ConvertBack(value, targetType, parameter, culture) = this.ConvertBack value targetType parameter culture
        abstract member Convert : (obj[] -> Type -> obj -> Globalization.CultureInfo -> obj)
        default this.Convert = convertFunction
        abstract member ConvertBack : (obj -> Type[] -> obj -> Globalization.CultureInfo -> obj[])
        default this.ConvertBack = convertBackFunction


type ValueOptionTypeConverter() =

    // from http://stackoverflow.com/questions/6289761
    let (|SomeObj|_|) =
      let ty = typedefof<option<_>>
      fun (a:obj) ->
        let aty = a.GetType()
        let v = aty.GetProperty("Value")
        if aty.IsGenericType && aty.GetGenericTypeDefinition() = ty then
          if a = null then None
          else Some(v.GetValue(a, [| |]))
        else None

    interface IValueConverter with
        //from source to binding target
        member x.Convert(value: obj, targetType: Type, parameter: obj, culture: CultureInfo) =
            match value with
            | null -> null
            | SomeObj(v) -> v
            | _ -> value
        //from binding target to source
        member x.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: CultureInfo) =
            match value with
            | null -> None :> obj
            | x -> Activator.CreateInstance(targetType, [| x |])

type IsSomeOptionTypeConverter() =
    interface IValueConverter with
        member x.Convert(value: obj, targetType: Type, parameter: obj, culture: CultureInfo) =
            match value with
            | null -> false :> _
            | _ -> true :> _

        member x.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: CultureInfo) =
            failwith "na" 



type FileExistsConverter() =
    interface IValueConverter with
        member x.Convert(value: obj, targetType: Type, parameter: obj, culture: CultureInfo) =
            System.IO.File.Exists(value :?> string) :> _

        member x.ConvertBack(value: obj, targetType: Type, parameter: obj, culture: CultureInfo) =
            failwith "na" 


type AndToDisplayConverter() =
    interface IMultiValueConverter with
      member x.Convert(values:obj[] , targetType: Type, parameter: obj, culture: CultureInfo) =
          box (values.Cast<bool>() |> Seq.forall id  |> boolToDisplay)

      member x.ConvertBack(value: obj, targetTypes: Type[], parameter: obj, culture: CultureInfo) =
         failwith "na" 

type OrToDisplayConverter() =
    interface IMultiValueConverter with
      member x.Convert(values:obj[] , targetType: Type, parameter: obj, culture: CultureInfo) =
          box (values.Cast<bool>() |> Seq.exists id |> boolToDisplay)

      member x.ConvertBack(value: obj, targetTypes: Type[], parameter: obj, culture: CultureInfo) =
         failwith "na" 

type AndConverter() =
    interface IMultiValueConverter with
      member x.Convert(values:obj[] , targetType: Type, parameter: obj, culture: CultureInfo) =
          values.Cast<bool>() |> Seq.forall id :> _

      member x.ConvertBack(value: obj, targetTypes: Type[], parameter: obj, culture: CultureInfo) =
         failwith "na" 

type OrConverter() =
    interface IMultiValueConverter with
      member x.Convert(values:obj[] , targetType: Type, parameter: obj, culture: CultureInfo) =
          values.Cast<bool>() |> Seq.exists id :> _

      member x.ConvertBack(value: obj, targetTypes: Type[], parameter: obj, culture: CultureInfo) =
         failwith "na" 

type NotBoolConverter() =
    inherit ConverterBase()
    override this.Convert     = fun (v:obj) _ _ _ -> box (v :?> bool |> not  )
    override this.ConvertBack = fun (v:obj) _ _ _ -> box (v :?> bool |> not)
            

type NotBoolToDisplayConverter() =
    inherit ConverterBase()
    override this.Convert     = fun (v:obj) _ _ _ -> box (v :?> bool |> not |> boolToDisplay)
    override this.ConvertBack = fun (v:obj) _ _ _ -> box (v :?> Visibility = Visibility.Visible |> not)
            

type BoolToDisplayConverter() =
    inherit ConverterBase()
    override this.Convert     = fun (v:obj) _ _ _ -> box (v :?> bool |> boolToDisplay)
    override this.ConvertBack = fun (v:obj) _ _ _ -> box (v :?> Visibility = Visibility.Visible)

type DebugConverter () =
    inherit ConverterBase()
    override this.Convert     =  fun value _ _ _ -> System.Diagnostics.Debugger.Break()
                                                    value
    override this.ConvertBack =  fun value _ _ _ -> System.Diagnostics.Debugger.Break()
                                                    value
type ListConverter() =
    inherit ConverterBase()
    override this.Convert     = fun (v:obj) _ _ _ ->  let sl = (v :?> string list) 
                                                      (if sl.IsEmpty then "" else sl |> List.reduce(fun s e -> s + "\n" + e ))  |> box 
    override this.ConvertBack = failwith "NA"


 type NavigateFileConverter ()=
    inherit ConverterBase()
    override this.Convert = fun value _ _ _  -> if value <> null then
                                                   String.Format(@"file://{0}",value) |> box
                                                else  "" |> box

    override this.ConvertBack = failwith "NA"

open System
open System.Text
open System.IO
open System.ComponentModel

type StringRedir() = 
  inherit StringWriter()

  let mutable _Text     = ""
  let event = Event<_, _>()  

  interface INotifyPropertyChanged with 
      member this.add_PropertyChanged(e) = event.Publish.AddHandler(e) 
      member this.remove_PropertyChanged(e) = event.Publish.RemoveHandler(e) 

  abstract Text : string  with get, set
  default this.Text with get() = _Text 
                    and set value = _Text <- value
                                    event.Trigger(this, new PropertyChangedEventArgs("Text"))
  member x.Reset() = _Text <- ""
  override x.WriteLine(m : string) = 
        x.Text <- x.Text + m + "\n"; 

