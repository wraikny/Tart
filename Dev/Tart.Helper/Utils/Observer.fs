namespace wraikny.Tart.Helper.Utils

[<Interface>]
type IObserver<'T> =
    abstract UpdateFromNotify : 'T -> unit


open System.Collections.Generic


[<AbstractClass>]
type Observable<'T>() =
    let observers = new List<IObserver<'T>>()

    member this.AddObserver(observer : #IObserver<'T>) =
        observers.Add(observer :> IObserver<'T>)


    member this.NotifyObservers(input) =
        observers.ForEach(fun o -> o.UpdateFromNotify(input))


    member this.ClearObservers() =
        observers.Clear()