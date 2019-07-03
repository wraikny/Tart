namespace wraikny.Tart.Helper.Utils

[<Interface>]
type IObserver<'T> =
    abstract Update : 'T -> unit


open System.Collections.Generic


[<AbstractClass>]
type Observable<'T>() =
    let observers = new List<IObserver<'T>>()

    member this.Add(observer : #IObserver<'T>) =
        observers.Add(observer :> IObserver<'T>)


    member this.Notify(input) =
        observers.ForEach(fun o -> o.Update(input))


    member this.Clear() =
        observers.Clear()