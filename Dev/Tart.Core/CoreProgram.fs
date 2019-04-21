namespace wraikny.Tart.Core
open wraikny.Tart.Core


type CoreProgram<'Msg, 'ViewMsg, 'Model, 'ViewModel> =
    {
        init : 'Model
        update : 'Msg -> 'Model -> ('Model * Cmd<'Msg, 'ViewMsg>)
        view : 'Model -> 'ViewModel
    }