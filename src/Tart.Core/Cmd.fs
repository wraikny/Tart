namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core.Libraries

open FSharpPlus

type TartConfig = {
    cts : System.Threading.CancellationTokenSource
    env : ITartEnv
} with
    static member SideEffect(a : Async<'Msg>, config : TartConfig) =
        fun dispatch ->
            Async.Start(async {
                let! msg = a
                dispatch msg
            }, config.cts.Token)

    static member SideEffect(generator : 'a Random.Generator, config : TartConfig) =
        generator.F config.env.Random



type Command<'Msg> = ('Msg -> unit) -> unit


type Cmd<'Msg, 'Port> = {
    commands : TartConfig -> Command<'Msg> list
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
        init (fun _ -> [(|>) msg]) []

    let inline ofPorts (m) = init (fun _ -> []) m

    let inline ofPort (m) = ofPorts [m]

    let inline batch (cmds : seq<Cmd<'Msg, 'Port>>) : Cmd<'Msg, 'Port> =
        {
            commands = fun config ->
                cmds
                |>> (getCommands >> (|>) config)
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
            commands = fun config ->
                cmd.commands(config)
                |>> fun c dispatch -> c(f >> dispatch)

            ports = cmd.ports
        }

    let inline mapPorts (f : 'a -> 'Port) (cmd : Cmd<'Msg, 'a>) : Cmd<'Msg, 'Port> =
        {
            commands = cmd.commands
            ports = f <!> cmd.ports
        }

    let inline private performSideEffect (x : ^``SideEffect<'a>``) (config : ^TartConfig) =
        ( (^``SideEffect<'a>`` or ^TartConfig) : (static member SideEffect : _*_->_) (x, config))

    let inline ofAsyncOption (a : Async<'a option>) =
        initCommand(fun config dispatch ->
            Async.Start(async {
                match! a with
                | Some x -> dispatch x
                | None -> ()
            }, config.cts.Token)
        )

    let inline sideEffect (x : ^``SideEffect<'a>``) =
        initCommand(performSideEffect x)

    let inline chain (f : 'a -> ^``SideEffect<'b>``) (x : ^``SideEffect<'a>``) : Cmd<'b, 'Port> =
        initCommand(fun config  ->
            performSideEffect x config
            |> f |> flip performSideEffect config
        )
