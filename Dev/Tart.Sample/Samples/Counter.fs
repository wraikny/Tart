module wraikny.Tart.Sample.Counter
open wraikny.Tart.Core

type Model = int

let init : Model = 0

type Msg =
    | Add of int
    | Sub of int
    | Clear


let update msg model : Model * Msg Cmd =
    msg |> function
    | Add i -> model + i, Cmd.none
    | Sub i -> model - i, Cmd.none
    | Clear -> 0, Cmd.none


type ViewModel = string


let view model : ViewModel =
    model.ToString()


let messengerBuilder() : IMessenger<Msg, ViewModel> =
    IMessenger.createMessenger {
        init = init
        update = update
        view = view
    }