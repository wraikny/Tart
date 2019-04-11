namespace wraikny.Tart.Core

type Cmd<'Msg, 'ViewMsg> =
    {
        commands : (('Msg -> unit) -> unit) list
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

    let internal singleCommand ( command : ('Msg -> unit) -> unit ) : Cmd<'Msg, 'ViewMsg> =
        init [command] []

    let pushViewMsgs (m) =
        init [] m

    /// Execute commands asynchronously
    let internal execute
        (sender : IMessageSender<'Msg>)
        (viewSender : IMessageSender<'ViewMsg> option)
        (cmd : Cmd<'Msg, 'ViewMsg>) =
        for c in cmd.commands do
            c(fun msg -> sender.PushMsg(msg))

        
        viewSender |> function
        | None -> ()
        | Some(viewSender) ->
            for msg in cmd.viewMsgs do
                viewSender.PushMsg(msg)


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
                    (fun pushMsg ->
                        c(f >> pushMsg)
                    )
                )

            viewMsgs = cmd.viewMsgs
        }