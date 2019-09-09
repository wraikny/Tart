namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils

open FSharpPlus

type CmdConfig =
    {
        cts : System.Threading.CancellationTokenSource
        env : IEnvironment
    }


type internal Command<'Msg> = ('Msg -> unit) -> unit

type Cmd<'Msg, 'ViewMsg> =
    {
        commands : CmdConfig -> Command<'Msg> list
        ports : 'ViewMsg list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cmd =
    let inline internal getCommands (cmd : Cmd<_, _>) = cmd.commands
    let inline internal getPorts (cmd : Cmd<_, _>) = cmd.ports

    let inline internal init (commands) (ports) : Cmd<'Msg, 'ViewMsg> =
        {
            commands = commands
            ports = ports
        }

    let inline internal initMsg ( command : _ -> Command<'Msg> ) : Cmd<'Msg, 'ViewMsg> =
        init (fun x -> [command x]) []

    [<CompiledName "Ports">]
    let ports (m) =
        init (fun _ -> []) m

    [<CompiledName "Port">]
    let inline port (m) = ports [m]


    let inline internal execute
        (msgQueue : #IEnqueue<'Msg>)
        (viewMsgQueue : #IEnqueue<'ViewMsg>)
        conf
        (cmd : Cmd<'Msg, 'ViewMsg>) =

        for c in (cmd.commands conf) do
            c msgQueue.Enqueue

        cmd.ports |> iter viewMsgQueue.Enqueue


    /// Empty command
    [<CompiledName "None">]
    let none : Cmd<'Msg, 'ViewMsg> = { commands = (fun _ -> []); ports = [] }


    [<CompiledName "Batch">]
    let batch (cmds : seq<Cmd<'Msg, 'ViewMsg>>) : Cmd<'Msg, 'ViewMsg> =
        {
            commands = fun env ->
                cmds
                |>> (getCommands >> (|>) env)
                |> Seq.concat
                |> Seq.toList
            ports =
                cmds
                |>> getPorts
                |> Seq.concat
                |> Seq.toList
        }


    [<CompiledName "MapCommands">]
    let mapMsgs (f : 'a -> 'Msg) (cmd : Cmd<'a, 'ViewMsg>) : Cmd<'Msg, 'ViewMsg> =
        {
            commands = fun env ->
                cmd.commands(env)
                |>> fun c -> ( fun pushMsg -> c(f >> pushMsg) )

            ports = cmd.ports
        }

    [<CompiledName "MapViewMsgs">]
    let inline mapPorts (f : 'a -> 'ViewMsg) (cmd : Cmd<'Msg, 'a>) : Cmd<'Msg, 'ViewMsg> =
        {
            commands = cmd.commands
            ports = f <!> cmd.ports
        }
