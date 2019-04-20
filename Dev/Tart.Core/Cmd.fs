namespace wraikny.Tart.Core

type internal PushMessage<'Msg> = 'Msg -> unit
type internal Command<'Msg> = IEnvironmentCore -> PushMessage<'Msg> -> unit

type Cmd<'Msg, 'ViewMsg> =
    {
        commands : Command<'Msg> list
        viewMsgs : 'ViewMsg list
    }


module Cmd =
    let internal commands (cmd : Cmd<_, _>) = cmd.commands
    let internal viewMsgs (cmd : Cmd<_, _>) = cmd.viewMsgs

    let internal init (commands) (viewCommands) : Cmd<'Msg, 'ViewMsg> =
        {
            commands = commands
            viewMsgs = viewCommands
        }

    let internal singleCommand ( command : Command<'Msg> ) : Cmd<'Msg, 'ViewMsg> =
        init [command] []

    let pushViewMsgs (m) =
        init [] m

    /// Execute commands asynchronously
    let internal execute
        (sender : IMsgSender<'Msg>)
        (env : Environment<'ViewMsg>)
        (cmd : Cmd<'Msg, 'ViewMsg>) =
        for c in cmd.commands do
            c (env :> IEnvironmentCore) (fun msg -> sender.PushMsg msg)

        
        env.Updater |> function
        | None -> ()
        | Some(sender) ->
            for msg in cmd.viewMsgs do
                sender.PushMsg(msg)


    /// Empty command
    let none : Cmd<'Msg, 'ViewMsg> = { commands = []; viewMsgs = [] }


    let batch (cmds : Cmd<'Msg, 'ViewMsg> list) : Cmd<'Msg, 'ViewMsg> =
        {
            commands =
                cmds
                |> List.map commands
                |> List.concat
            viewMsgs =
                cmds
                |> List.map viewMsgs
                |> List.concat
        }


    let map(f : 'a -> 'Msg) (cmd : Cmd<'a, 'ViewMsg>) : Cmd<'Msg, 'ViewMsg> =
        {
            commands =
                cmd.commands
                |> List.map(fun c ->
                    (fun env pushMsg ->
                        c env (f >> pushMsg)
                    )
                )

            viewMsgs = cmd.viewMsgs
        }