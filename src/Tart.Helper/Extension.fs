[<AutoOpen>]
module wraikny.Tart.Helper.Extension

let inline pure' (x: 'a): ^``M<'a>`` =
    (^``M<'a>``: (static member Return: _->_) x)

let inline ifThen cond f = if cond then f else id
   
//let inline increment x = x + La

//let inline decrement x = x - one

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