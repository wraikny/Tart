namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils

open FSharpPlus

type internal Command<'Msg> = ('Msg -> unit) -> unit

type Cmd<'Msg, 'ViewMsg> =
    {
        commands : IEnvironment -> Command<'Msg> list
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

    let inline internal initMsg ( command : IEnvironment -> Command<'Msg> ) : Cmd<'Msg, 'ViewMsg> =
        init (fun env -> [command env]) []

    [<CompiledName "Ports">]
    let ports (m) =
        init (fun _ -> []) m

    [<CompiledName "Port">]
    let inline port (m) = ports [m]


    let inline internal execute
        (msgQueue : #IEnqueue<'Msg>)
        (viewMsgQueue : #IEnqueue<'ViewMsg>)
        (env : IEnvironment)
        (cmd : Cmd<'Msg, 'ViewMsg>) =

        for c in (cmd.commands env) do
            c msgQueue.Enqueue

        cmd.ports |> iter viewMsgQueue.Enqueue


    /// Empty command
    [<CompiledName "None">]
    let none : Cmd<'Msg, 'ViewMsg> = { commands = (fun _ -> []); ports = [] }


    [<CompiledName "Batch">]
    let inline batch (cmds : Cmd<'Msg, 'ViewMsg> list) : Cmd<'Msg, 'ViewMsg> =
        {
            commands = fun env ->
                cmds
                |>> (getCommands >> (|>) env)
                |> join
            ports =
                cmds
                |>> getPorts
                |> join
        }


    [<CompiledName "MapCommands">]
    let inline mapMsgs (f : 'a -> 'Msg) (cmd : Cmd<'a, 'ViewMsg>) : Cmd<'Msg, 'ViewMsg> =
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
