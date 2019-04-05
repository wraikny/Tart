namespace wraikny.Tart.Core.Cmd

type 'Msg Cmd =
    {
        commands : (unit -> 'Msg) list
    }


module Cmd =
    let internal commands (cmd : 'Msg Cmd) = cmd.commands

    /// Execute commands asynchronously
    let internal exe (pushMsg) (cmd : 'Msg Cmd) =
        if cmd.commands |> List.isEmpty then ()
        else
            async {
                cmd.commands
                |> List.map(fun c -> async{ c() |> pushMsg })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore
            }
            |> Async.StartImmediate

    /// Empty command
    let none : 'Msg Cmd = { commands = [] }


    let batch (cmds : 'Msg Cmd list) : 'Msg Cmd =
        {
            commands =
                cmds
                |> List.map commands
                |> List.concat
        }


    let map(f : 'a -> 'Msg) (cmd : 'a Cmd) : 'Msg Cmd =
        {
            commands =
                cmd.commands
                |> List.map(fun c -> c >> f )
        }