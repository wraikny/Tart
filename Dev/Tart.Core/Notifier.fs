namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils

type Notifier<'Msg, 'ViewMsg, 'ViewModel>(messenger) =
    inherit Observable<'ViewModel>()

    member val Messenger : IMessenger<'Msg, 'ViewMsg, 'ViewModel> = messenger with get

    member this.Pull() =
        this.Messenger.TryPopViewModel |> function
        | Some viewModel ->
            this.Notify(viewModel)
            true
        | None ->
            false