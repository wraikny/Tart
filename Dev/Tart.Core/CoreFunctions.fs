namespace wraikny.Tart.Core
open wraikny.Tart.Core


type CoreFunctions<'Model, 'Msg, 'ViewModel when 'Model : struct> =
    {
        init : 'Model
        update : 'Msg -> 'Model -> ('Model * 'Msg Cmd)
        view : 'Model -> 'ViewModel
    }