namespace FSharp.Windows

//open System.Collections.Generic
open System.Collections.ObjectModel
open System.ComponentModel
open System.Windows

open System.ComponentModel
open Microsoft.FSharp.Quotations.Patterns
open System
 

type ObservableObject () =
    //inherit FrameworkElement()

    let propertyChanged = 
        Event<PropertyChangedEventHandler,PropertyChangedEventArgs>()
    let getPropertyName = function 
        | PropertyGet(_,pi,_) -> pi.Name
        | _ -> invalidOp "Expecting property getter expression"
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChanged.Publish
    member this.NotifyPropertyChanged propertyName = 
        propertyChanged.Trigger(this,PropertyChangedEventArgs(propertyName))
    member this.NotifyPropertyChanged quotation = 
        quotation |> getPropertyName |> this.NotifyPropertyChanged
