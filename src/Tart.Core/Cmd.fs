namespace wraikny.Tart.Core

open wraikny.Tart.Core.Libraries
open FSharpPlus

type Runtime = {
    env : ITartEnv
    cts : System.Threading.CancellationTokenSource
    onError : exn -> unit
}

type SideEffect<'a, 'b> = SideEffect of (Runtime -> ('a -> 'b) -> 'b)
with
    member inline internal x.Apply(a, d) = x |> function SideEffect f -> f a d


type Runtime with
    static member inline SideEffect(x : SideEffect<'a,'b>, runtime, dispatch : 'a -> 'b) =
        x.Apply(runtime, dispatch)

    static member inline SideEffect(x : Async<'a option>, runtime : Runtime, dispatch : 'a -> unit) =
        Async.Start(async{
            try
                match! x with
                | Some s -> dispatch s
                | None -> ()
            with e ->
                runtime.onError e
                
        }, runtime.cts.Token)

    static member inline SideEffect(x : Async<Result<'a, exn>>, runtime : Runtime, dispatch : Result<'a, exn> -> unit) =
        Async.Start(async{
            try
                let! s = x in dispatch s
            with e ->
                dispatch(Error e)
                
        }, runtime.cts.Token)

    //static member inline SideEffect(x : Async<'a>, runtime : Runtime, dispatch : Result<'a, exn> -> unit) =
    //    Async.Start(async{
    //        try
    //            let! s = x
    //            dispatch (Ok s)
    //        with e ->
    //            dispatch(Error e)
                
    //    }, runtime.cts.Token)

    static member SideEffect(generator : 'a Random.Generator, runtime, dispatch : 'a -> 'b) =
        (generator.F runtime.env.Random) |> dispatch

    static member SideEffect(storage : 'a File.StorageIO, _runtime, dispatch) =
        (storage.F()) |> dispatch


type Cmd<'Msg, 'Port> = {
    commands : Runtime -> (('Msg -> unit) -> unit) list
    ports : 'Port list
} with
    static member Zero : Cmd<'Msg, 'Port> = {
        commands = fun _ -> []
        ports = []
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cmd =
    let inline private init commands ports : Cmd<'Msg, 'Port> =
        { commands = commands; ports = ports }

    let inline internal execute
        (msgDispatch : 'Msg -> unit) (portDispatch : 'Port -> unit)
        runtime (cmd : Cmd<'Msg, 'Port>) =

        for c in (cmd.commands runtime) do c msgDispatch
        cmd.ports |> iter portDispatch


    /// Empty command
    let none : Cmd<'Msg, 'Port> = { commands = (fun _ -> []); ports = [] }

    let inline ofMsgs (msgs : seq<'Msg>) : Cmd<'Msg, 'Port> =
        init (fun _ -> [ for msg in msgs -> (|>) msg ]) []

    let inline ofMsg (msg : 'Msg) : Cmd<'Msg, 'Port> =
        init (fun _ -> [(|>) msg]) []

    let inline ofPorts m : Cmd<'Msg, 'Port> = init (fun _ -> []) m

    let inline ofPort m : Cmd<'Msg, 'Port> = ofPorts [m]

    let inline batch (cmds : seq<Cmd<'Msg, 'Port>>) : Cmd<'Msg, 'Port> =
        {
            commands = fun runtime ->
                [ for c in cmds do yield! (c.commands runtime)]

            ports = [ for c in cmds do yield! c.ports ]
        }

    let inline mapMsgs (f : 'a -> 'Msg) (cmd : Cmd<'a, 'Port>) : Cmd<'Msg, 'Port> =
        {
            commands = fun runtime ->
                cmd.commands runtime
                |>> fun c dispatch -> f >> dispatch |> c

            ports = cmd.ports
        }

    let inline mapPorts (f : 'a -> 'Port) (cmd : Cmd<'Msg, 'a>) : Cmd<'Msg, 'Port> =
        {
            commands = cmd.commands
            ports = f <!> cmd.ports
        }


module SideEffect =
    let inline private init c = { commands = (fun x -> [c x]); ports = [] }

    let inline private invoke (runtime : ^Runtime) (dispatch : 'a -> 'b) (x : ^``SideEffect<'a>``) : 'b =
        ((^``SideEffect<'a>`` or ^Runtime) : (static member SideEffect : _*_*_->_) (x, runtime, dispatch))
    
    let inline map (f : 'a -> 'b) (x : '``SideEffect<'a>``) : SideEffect<'b, 'c> =
        SideEffect <| fun runtime dispatch ->
            invoke runtime (f >> dispatch) x
    
    let inline bind (f : 'a -> '``SideEffect<'b>``) (x : '``SideEffect<'a>``) : SideEffect<'b, 'c> =
        SideEffect <| fun runtime dispatch ->
            invoke runtime f x
            |> invoke runtime dispatch
    
    let inline unwrapAsync (x : '``SideEffect<Async<'a>>``) =
        init <| fun runtime dispatch ->
            Async.Start(async {
                try
                    let! a = invoke runtime id x
                    dispatch(a)
                with e ->
                    runtime.onError e
            }, runtime.cts.Token)

    let inline perform (x : '``SideEffect<'a>``) : Cmd<'Msg, 'Port> =
        init <| fun runtime dispatch -> invoke runtime dispatch x
    
    let inline performWith (f : 'a -> 'Msg) (x : ^``SideEffect<'a>``) : Cmd<'Msg, 'Port> =
        x |> map f |> perform


[<AutoOpen>]
module SideEffectExt =
    type SideEffectBuilder() =
        member inline __.Return x =  SideEffect(fun _ dispatch -> dispatch x)

        member inline __.ReturnFrom(x) = x

        member inline __.Bind(x, f) = SideEffect.bind f x
        member inline __.Zero() = SideEffect(fun _ _ -> ())
        member inline __.For(inp,f) =
            seq {for a in inp -> f a}
        member inline __.Delay(f) = f()

        //member __.Combine(x, f) = g >> f

        member inline __.Using(x: #System.IDisposable, f: _ -> _) =
            try f x
            finally
                match box x with
                | null -> () | _ -> x.Dispose()

        member inline __.TryWith(f, h) = try f () with e -> h e
        member inline __.TryFinally(f, g) = try f () finally g ()
        //member this.While(guard, f) =
        //    if not (guard ()) then this.Zero()
        //    else let x = f () in this.Combine(x, this.While(guard, f))

    let sideEffect = SideEffectBuilder()
