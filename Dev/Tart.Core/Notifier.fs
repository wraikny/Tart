namespace wraikny.Tart.Core

open wraikny.Tart.Helper

type Notifier<'Msg, 'ViewMsg, 'ViewModel>(messenger) =
    inherit Observable<'ViewModel>()

    member val Messenger : IMessenger<'Msg, 'ViewModel> = messenger with get

    member this.Update() =
        messenger.TryViewModel |> function
        | Some viewModel ->
            this.NotifyObservers(viewModel)
        | None -> ()