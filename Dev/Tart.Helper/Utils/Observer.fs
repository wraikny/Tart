namespace wraikny.Tart.Helper.Utils


type IObserver<'T> = interface
    abstract Update : 'T -> unit
end


type IObservable<'T> = interface
    abstract Add : #IObserver<'T> -> bool
    abstract Clear : unit -> unit
end


// open System
open System.Linq
open System.Collections.Generic


type Observable<'T>() =
    let observers = new HashSet<IObserver<'T>>()

    member this.Add(observer : #IObserver<'T>) =
        if observers.Contains(observer) then
            raise <| System.ArgumentException("is already added")
        observers.Add(observer)


    member this.Notify(input : 'T) =
        for o in observers do
            o.Update(input)


    member this.Clear() =
        observers.Clear()

    interface IObservable<'T> with
        member this.Add(o) = this.Add(o)

        member this.Clear() = this.Clear()