namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core.Libraries

open FSharpPlus

type Command<'Msg> = ('Msg -> unit) -> unit

module private SideEffectChain =
    let inline perform (runtime : ^Runtime) (dispatch : 'a -> 'b) (x : ^``SideEffect<'a>``) : 'b =
        ( (^``SideEffect<'a>`` or ^Runtime) : (static member SideEffect : _*_*_->_) (x, runtime, dispatch))


type SideEffectChain<'``SideEffect<'a>``> = SideEffectChain of (Runtime -> '``SideEffect<'a>``)
with
    member inline x.Apply(a) = x |> function SideEffectChain f -> f a


and Runtime = {
    env : ITartEnv
    cts : System.Threading.CancellationTokenSource
    onError : exn -> unit
} with
    static member inline SideEffect(x : Async<'a option>, runtime : Runtime, dispatch : 'a -> unit) =
        Async.Start(async{
            try
                match! x with
                | Some s -> dispatch s
                | None -> ()
            with e ->
                runtime.onError e
                
        }, runtime.cts.Token)

    static member inline SideEffect(x : Async<Result<'a, 'e>>, runtime : Runtime, dispatch : Result<'a, 'e> -> unit) =
        Async.Start(async{
            try
                let! s = x
                dispatch s
            with e -> runtime.onError e
        }, runtime.cts.Token)

    static member inline SideEffect(x : Async<'a>, runtime : Runtime, dispatch : Result<'a, exn> -> unit) =
        Async.Start(async{
            try
                let! s = x
                dispatch (Ok s)
            with e ->
                dispatch(Error e)
                
        }, runtime.cts.Token)

    //static member inline SideEffect(x : Async<'a>, runtime : TartConfig, dispatch : 'a -> unit) =
    //    Async.Start(async{
    //        try
    //            let! s = x
    //            dispatch s
    //        with e ->
    //            runtime.onError e
                
    //    }, runtime.cts.Token)

    static member SideEffect(generator : 'a Random.Generator, runtime, dispatch : 'a -> 'b) =
        (generator.F runtime.env.Random) |> dispatch

    static member inline SideEffect(x : SideEffectChain<'``SideEffect<'a>``>, runtime, dispatch : 'a -> 'b) =
        SideEffectChain.perform runtime dispatch (x.Apply runtime)


type Cmd<'Msg, 'Port> = {
    commands : Runtime -> Command<'Msg> list
    ports : 'Port list
} with
    static member Zero : Cmd<'Msg, 'Port> = {
        commands = fun _ -> []
        ports = []
    }


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

module SideEffect =
    let inline private init c = { commands = (fun x -> [c x]); ports = [] }

    let inline perform (x : ^``SideEffect<'Msg>``) : Cmd<'Msg, 'Port> =
        init <| fun runtime dispatch -> SideEffectChain.perform runtime dispatch x

    let inline performWith (f : 'a -> 'Msg) (x : ^``SideEffect<'a>``) : Cmd<'Msg, 'Port> =
        init <| fun runtime dispatch -> SideEffectChain.perform runtime (f >> dispatch) x

    let inline bind (f : 'a -> '``SideEffect<'b>``) (x : '``SideEffect<'a>``) : SideEffectChain<'``SideEffect<'b>``> =
        SideEffectChain(fun runtime -> SideEffectChain.perform runtime f x)

    let inline unwrapAsync (x : '``SideEffect<Async<'a>>``) =
        init <| fun runtime dispatch ->
            Async.Start(async {
                try
                    let! a = SideEffectChain.perform runtime id x
                    dispatch(a)
                with e ->
                    runtime.onError e
            }, runtime.cts.Token)
