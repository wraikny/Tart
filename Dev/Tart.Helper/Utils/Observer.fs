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


type Observable<'T>() = class
    let observers = new HashSet<IObserver<'T>>()

    member __.Notify(input : 'T) =
        for o in observers do
            o.Update(input)

    interface IObservable<'T> with
        member __.Add(observer : #IObserver<'T>) =
            if observers.Contains(observer) then
                raise <| System.ArgumentException("is already added")
            observers.Add(observer)

        member __.Clear() = observers.Clear()
end