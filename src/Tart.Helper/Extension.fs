[<AutoOpen>]
module wraikny.Tart.Helper.Extension

open FSharpPlus

let inline pure' (x : ^a) =
    (^a : (static member Return : ^a -> '``M<'a>``) x)

let inline map2
    (f : 'a -> 'b -> 'c)
    (a : '``Applicative<'a>``)
    (b : '``Applicative<'b>``)
    : '``Applicative<'c>`` = f <!> a <*> b

let inline indexed (source : '``Functor<'a>``) : '``Functor<int * 'a>`` =
    let mutable i = -1
    source
    |>> (fun x ->
        i <- i + 1
        (i, x)
    )

let inline ifThen cond f = if cond then f else id
    
let inline increment x = x + one

let inline decrement x = x - one

module Async =
    let inline toOption (a : Async<'a>) : Async<'a option> =
        async {
            try
                let! r = a
                return Some r
            with _ ->
                return None
        }

    let inline toResult (a : Async<'a>) : Async<Result<'a, exn>> =
        async {
            try
                let! r = a
                return Ok r
            with e ->
                return Error e
        }

module Bytes =
    let inline encode (x : ^a) =
        (^a : (member Encode : unit -> byte []) x)
    
    let inline decode (bytes : byte []) : ^a option =
        (^a : (static member Decode : _ -> _) bytes)