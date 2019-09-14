namespace wraikny.Tart.Helper.Utils

module Bytes =
    let inline encode (x : ^a) =
        (^a : (member Encode : unit -> byte []) x)
    
    let inline decode (bytes : byte []) : ^a option =
        (^a : (static member Decode : _ -> _) bytes)


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

    abstract Clear : unit -> unit


module Async =
    [<CompiledName "ToOption">]
    let toOption (a : Async<'a>) : Async<'a option> =
        async {
            try
                let! r = a
                return Some r
            with _ ->
                return None
        }

    [<CompiledName "ToResult">]
    let toResult (a : Async<'a>) : Async<Result<'a, exn>> =
        async {
            try
                let! r = a
                return Ok r
            with e ->
                return Error e
        }