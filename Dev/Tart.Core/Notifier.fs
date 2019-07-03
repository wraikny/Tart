namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils

type Notifier<'Msg, 'ViewMsg, 'ViewModel>(messenger) =

    let observable = new Observable<'ViewModel>()

    interface IObservable<'ViewModel> with
        member __.Add(o) = observable.Add(o)
        member __.Clear() = observable.Clear()

    member val Messenger : IMessenger<'Msg, 'ViewMsg, 'ViewModel> = messenger with get

    member this.Pull() =
        this.Messenger.TryPopViewModel |> function
        | Some viewModel ->
            observable.Notify(viewModel)
            true
        | None ->
            false
