namespace FSharp.Windows

open System
open System.Windows
open System.Windows.Controls

type IView<'Events, 'Model> = 
    inherit IObservable<'Events>

    abstract SetBindings : 'Model -> unit

[<AbstractClass>]
type View<'Events, 'Model, 'Control when 'Control :> FrameworkElement>(control : 'Control) as this =
    let mutable isOK = false

    member this.Control = control
    static member (?) (view : View<'Events,'Model, 'Control>, name) = 
        match view.Control.FindName name with
        | null -> 
            match view.Control.TryFindResource name with
            | null -> invalidArg "Name" ("Cannot find child control or resource named: " + name)
            | resource -> resource |> unbox
        | control -> control |> unbox
    
    interface IView<'Events,'Model> with

        member this.Subscribe observer = 
            let xs = this.EventStreams |> List.reduce Observable.merge 
            xs.Subscribe observer
        member this.SetBindings model = 
            control.DataContext <- model
            this.SetBindings model

    interface IDisposable with
      member x.Dispose() = 
         this.Dispose()

    abstract Dispose : unit  -> unit
    abstract EventStreams : IObservable<'Events> list
    abstract SetBindings : 'Model -> unit

