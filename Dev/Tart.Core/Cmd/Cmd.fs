﻿namespace wraikny.Tart.Core.Cmd

type 'Msg Cmd =
    {
        commands : (('Msg -> unit) -> unit) list
    }


module Cmd =
    let internal commands (cmd : 'Msg Cmd) = cmd.commands

    /// Execute commands asynchronously
    let internal exe (pushMsg : 'Msg -> unit) (cmd : 'Msg Cmd) =
        if cmd.commands |> List.isEmpty then ()
        else
            for c in cmd.commands do
                c(pushMsg)

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
                |> List.map(fun c ->
                    (fun pushMsg ->
                        c(f >> pushMsg)
                    )
                )
        }