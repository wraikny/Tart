module wraikny.Tart.Sample.Counter
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries

type Model =
    {
        count : int
        tmp : int
    }

type ViewModel = Model

type Msg =
    | Add of int
    | Sub of int
    | Clear
    | Random of int * int
    | NewValue of int

let setCount i model = { model with count = i }
let addCount i model = setCount (model.count + i) model

let update msg model : Model * Cmd<_, _> =
    msg |> function
    | Add i ->
        model |> addCount i, Cmd.none
    | Sub i ->
        model |> addCount (-i), Cmd.none
    | Clear ->
        model |> setCount 0, Cmd.none
    | Random (a, b) ->
        model, (Random.generate NewValue (Random.int a b))
    | NewValue i ->
        model |> setCount i, Cmd.none


let view model : ViewModel =
    model


let init : Model * Cmd<Msg, _> =
    {
        count = 0
        tmp = 0
    }, Cmd.none


let messengerBuilder() : IMessenger<Msg, _, ViewModel> =
    Messenger.buildMessenger
        {
            seed = 0
        }
        {
            init = init
            update = update
            view = view
        }