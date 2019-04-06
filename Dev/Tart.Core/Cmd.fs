namespace wraikny.Tart.Core

type 'msg Cmd =
    {
        commands : (('msg -> unit) -> unit) list
    }


module Cmd =
    let internal commands (cmd : 'msg Cmd) = cmd.commands

    let internal init (commands) : 'msg Cmd =
        {
            commands = commands
        }

    let internal singleCommand ( command : ('msg -> unit) -> unit ) : 'msg Cmd =
        init([command])

    /// Execute commands asynchronously
    let internal execute (pushMsg : 'msg -> unit) (cmd : 'msg Cmd) =
        for c in cmd.commands do
            c(pushMsg)

    /// Empty command
    let none : 'msg Cmd = { commands = [] }


    let batch (cmds : 'msg Cmd list) : 'msg Cmd =
        {
            commands =
                cmds
                |> List.map commands
                |> List.concat
        }


    let map(f : 'a -> 'msg) (cmd : 'a Cmd) : 'msg Cmd =
        {
            commands =
                cmd.commands
                |> List.map(fun c ->
                    (fun pushMsg ->
                        c(f >> pushMsg)
                    )
                )
        }