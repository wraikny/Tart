namespace wraikny.Tart.Core
open wraikny.Tart.Core

[<Struct>]
type CoreProgram<'Msg, 'ViewMsg, 'Model, 'ViewModel> =
    {
        init : 'Model * Cmd<'Msg, 'ViewMsg>
        update : 'Msg -> 'Model -> ('Model * Cmd<'Msg, 'ViewMsg>)
        view : 'Model -> 'ViewModel
    }