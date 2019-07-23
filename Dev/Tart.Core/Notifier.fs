namespace wraikny.Tart.Core

open System
open System.Reactive

open wraikny.Tart.Helper.Utils

type Notifier<'Msg, 'ViewMsg, 'ViewModel>(messenger) =

    let subject = new Subjects.Subject<'ViewModel>()

    member val Messenger : IMessenger<'Msg, 'ViewMsg, 'ViewModel> = messenger with get

    member this.Pull() =
        this.Messenger.TryPopViewModel |> function
        | Some viewModel ->
            subject.OnNext(viewModel)
            true
        | None ->
            false

    member __.Subscribe(observer) = subject.Subscribe(observer)
    member __.Dispose() = subject.Dispose()

    interface IObservable<'ViewModel> with
        member this.Subscribe(observer) = this.Subscribe(observer)

    interface IDisposable with
        member this.Dispose() = this.Dispose()