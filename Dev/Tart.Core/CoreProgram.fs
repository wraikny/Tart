namespace wraikny.Tart.Core
open wraikny.Tart.Core


type CoreProgram<'Msg, 'Model, 'ViewModel> =
    {
        init : 'Model
        update : 'Msg -> 'Model -> ('Model * 'Msg Cmd)
        view : 'Model -> 'ViewModel
    }