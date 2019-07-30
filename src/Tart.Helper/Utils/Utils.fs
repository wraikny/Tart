namespace wraikny.Tart.Helper.Utils

[<Class>]
type LockObject<'T>(value : 'T) =
    let mutable value = value
    let _lock = System.Object()

    member public __.Value
        with get() =
            lock _lock <| fun _ ->
                value

        and set(value_) =
            lock _lock <| fun _ ->
                value <- value_


//[<AbstractClass; Sealed>]
//type StaticLock private() =
//    static member private _lockObj = System.Object()

//    static member Lock f =
//        lock StaticLock._lockObj <|
//            fun _ -> f()


type IUpdatee<'a> =
    abstract Update : 'a -> unit


open System.Collections.Generic

type IEnqueue<'T> =
    abstract Enqueue : 'T -> unit


type IDequeue<'T> =
    abstract TryDequeue : unit -> 'T option


type IQueue<'T> =
    inherit IEnqueue<'T>
    inherit IDequeue<'T>
    inherit IReadOnlyCollection<'T>