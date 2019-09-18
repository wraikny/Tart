namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core.Libraries

open FSharpPlus

type Command<'Msg> = ('Msg -> unit) -> unit

type Runtime = {
    env : ITartEnv
    cts : System.Threading.CancellationTokenSource
    onError : exn -> unit
}

type SideEffect<'a, 'b> = SideEffect of (Runtime -> ('a -> 'b) -> 'b)
with
    member inline x.Apply(a, d) = x |> function SideEffect f -> f a d

type Cmd<'Msg, 'Port> = {
    commands : Runtime -> Command<'Msg> list
    ports : 'Port list
} with
    static member Zero : Cmd<'Msg, 'Port> = {
        commands = fun _ -> []
        ports = []
    }

module private SideEffectChain =
    let inline perform (runtime : ^Runtime) (dispatch : 'a -> 'b) (x : ^``SideEffect<'a>``) : 'b =
        ( (^``SideEffect<'a>`` or ^Runtime) : (static member SideEffect : _*_*_->_) (x, runtime, dispatch))

    let inline constraint'< ^Runtime, ^``SideEffect<'a>``, 'a, 'b
                        when (^``SideEffect<'a>`` or ^Runtime) :
                            (static member SideEffect : ^``SideEffect<'a>`` * ^Runtime * ('a -> 'b)->'a) > = ()


module SideEffect =
    let inline private init c = { commands = (fun x -> [c x]); ports = [] }
    
    let inline performWith (f : 'a -> 'Msg) (x : ^``SideEffect<'a>``) : Cmd<'Msg, 'Port> =
        init <| fun runtime dispatch -> SideEffectChain.perform runtime (f >> dispatch) x
    
    let inline perform (x : ^``SideEffect<'Msg>``) : Cmd<'Msg, 'Port> = performWith id x
    
    let inline map (f : 'a -> 'b) (x : '``SideEffect<'a>``) : SideEffect<'b, 'c> =
        SideEffect(fun runtime dispatch ->
            SideEffectChain.perform runtime (f >> dispatch) x
        )
    
    let inline bind (f : 'a -> '``SideEffect<'b>``) (x : '``SideEffect<'a>``) : SideEffect<'b, 'c> =
        SideEffect(fun runtime dispatch ->
            SideEffectChain.perform runtime f x
            |> SideEffectChain.perform runtime dispatch
        )
    
    let inline unwrapAsync (x : '``SideEffect<Async<'a>>``) =
        init <| fun runtime dispatch ->
            Async.Start(async {
                try
                    let! a = SideEffectChain.perform runtime id x
                    dispatch(a)
                with e ->
                    runtime.onError e
            }, runtime.cts.Token)


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
                let! s = x
                dispatch s
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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cmd =
    let inline private getCommands (cmd : Cmd<_, _>) = cmd.commands
    let inline private getPorts (cmd : Cmd<_, _>) = cmd.ports

    let inline private init (commands) (ports) : Cmd<'Msg, 'Port> =
        {
            commands = commands
            ports = ports
        }

    let inline private initCommand c = init (fun x -> [c x]) []

    let inline internal execute
        (msgDispatch : 'Msg -> unit)
        (portDispatch : 'Port -> unit)
        conf
        (cmd : Cmd<'Msg, 'Port>) =

        for c in (cmd.commands conf) do
            c msgDispatch

        cmd.ports |> iter portDispatch


    /// Empty command
    let none : Cmd<'Msg, 'Port> = { commands = (fun _ -> []); ports = [] }

    let inline ofMsgs (msgs : seq<'Msg>) : Cmd<'Msg, 'Port> =
        init (fun _ -> Seq.map (|>) msgs |> toList) []

    let inline ofMsg (msg : 'Msg) : Cmd<'Msg, 'Port> =
        initCommand (fun _ -> (|>) msg)

    let inline ofPorts (m) = init (fun _ -> []) m

    let inline ofPort (m) = ofPorts [m]

    let inline batch (cmds : seq<Cmd<'Msg, 'Port>>) : Cmd<'Msg, 'Port> =
        {
            commands = fun runtime ->
                cmds
                |>> (getCommands >> (|>) runtime)
                |> Seq.concat
                |> Seq.toList
            ports =
                cmds
                |>> getPorts
                |> Seq.concat
                |> Seq.toList
        }

    let inline mapMsgs (f : 'a -> 'Msg) (cmd : Cmd<'a, 'Port>) : Cmd<'Msg, 'Port> =
        {
            commands = fun runtime ->
                cmd.commands(runtime)
                |>> fun c dispatch -> c(f >> dispatch)

            ports = cmd.ports
        }

    let inline mapPorts (f : 'a -> 'Port) (cmd : Cmd<'Msg, 'a>) : Cmd<'Msg, 'Port> =
        {
            commands = cmd.commands
            ports = f <!> cmd.ports
        }

    type SideEffectBuilder() =
        member __.Return x =  SideEffect(fun _ dispatch -> dispatch x)
        member __.ReturnFrom(x) = x
        member inline __.Bind(x, f) = SideEffect.bind f x
        member __.For(inp,f) =
            seq {for a in inp -> f a}
        member __.Zero() = SideEffect(fun _ dispatch -> dispatch())
        member __.Delay(f) = f()

    let sideEffect = SideEffectBuilder()

    //let x() =
    //    Random.bool
    //    |> SideEffect.bind(fun x -> Random.bool)
    //    |> SideEffect.bind(fun x -> Random.double01)

    //let a() =
    //    sideEffect {
    //        let! x = Random.bool
    //        let! f = Random.double01
    //        let! f = Random.double01
    //        let! f = Random.double01
    //        let! f = Random.double01
    //        let! x = File.IsolatedStorage.readTextAsync "aaa"
    //        return! x
    //    }