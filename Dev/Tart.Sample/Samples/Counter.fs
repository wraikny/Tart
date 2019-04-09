module wraikny.Tart.Sample.Counter
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries

type Model = int

let init : Model = 0

type Msg =
    | Add of int
    | Sub of int
    | Clear
    | Random of int * int
    | NewValue of int


let update msg model : Model * Msg Cmd =
    msg |> function
    | Add i -> model + i, Cmd.none
    | Sub i -> model - i, Cmd.none
    | Clear -> 0, Cmd.none
    | Random (a, b) -> model, (Random.generate NewValue (Random.int a b))
    | NewValue i -> i, Cmd.none


type ViewModel = Model


let view model : ViewModel =
    model


let messengerBuilder() : IMessenger<Msg, ViewModel> =
    IMessenger.createMessenger {
        init = init
        update = update
        view = view
    }