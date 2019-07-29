namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils

open FSharpPlus

type internal PushMessage<'Msg> = 'Msg -> unit
type internal Command<'Msg> = IEnvironment -> PushMessage<'Msg> -> unit

type Cmd<'Msg, 'ViewMsg> =
    {
        commands : Command<'Msg> list
        viewMsgs : 'ViewMsg list
    }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cmd =
    let inline internal commands (cmd : Cmd<_, _>) = cmd.commands
    let inline internal viewMsgs (cmd : Cmd<_, _>) = cmd.viewMsgs

    let inline internal init (commands) (viewCommands) : Cmd<'Msg, 'ViewMsg> =
        {
            commands = commands
            viewMsgs = viewCommands
        }

    let inline internal singleCommand ( command : Command<'Msg> ) : Cmd<'Msg, 'ViewMsg> =
        init [command] []

    [<CompiledName "ViewMsg">]
    let viewMsg (m) =
        init [] m


    let inline internal execute
        (messenger : #IMsgQueue<'Msg>)
        (port : #IMsgQueue<'ViewMsg> option)
        (env : #IEnvironment)
        (cmd : Cmd<'Msg, 'ViewMsg>) =
        for c in cmd.commands do
            c env <| fun msg -> messenger.Enqueue msg

        
        port |> function
        | None -> ()
        | Some(port) ->
            for msg in cmd.viewMsgs do
                port.Enqueue(msg)


    /// Empty command
    [<CompiledName "None">]
    let none : Cmd<'Msg, 'ViewMsg> = { commands = []; viewMsgs = [] }


    [<CompiledName "Batch">]
    let batch (cmds : Cmd<'Msg, 'ViewMsg> list) : Cmd<'Msg, 'ViewMsg> =
        {
            commands =
                cmds
                |>> commands
                |> join
            viewMsgs =
                cmds
                |>> viewMsgs
                |> join
        }


    [<CompiledName "MapCommands">]
    let mapCommands (f : 'a -> 'Msg) (cmd : Cmd<'a, 'ViewMsg>) : Cmd<'Msg, 'ViewMsg> =
        {
            commands =
                cmd.commands
                |>> fun c ->
                    (fun env pushMsg ->
                        c env (f >> pushMsg)
                    )

            viewMsgs = cmd.viewMsgs
        }

    [<CompiledName "MapViewMsgs">]
    let inline mapViewMsgs (f : 'a -> 'ViewMsg) (cmd : Cmd<'Msg, 'a>) : Cmd<'Msg, 'ViewMsg> =
        {
            commands = cmd.commands
            viewMsgs = f <!> cmd.viewMsgs
        }