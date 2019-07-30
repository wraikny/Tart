namespace wraikny.Tart.Core

open System
open System.Reactive

open wraikny.Tart.Helper.Utils

open FSharpPlus

type Notifier<'T>(queue : IDequeue<'T>) =
    let subject = new Subjects.Subject<'T>()

    member __.Pull() =
        queue.TryDequeue()
        |> iter subject.OnNext

    member __.PullAll() =
        let rec loop () =
            queue.TryDequeue() |> function
            | Some x ->
                subject.OnNext x
                loop()
            | None -> ()

        loop ()

    member __.Subscribe(observer) = subject.Subscribe(observer)
    member __.Dispose() = subject.Dispose()

    interface IObservable<'T> with
        member this.Subscribe(observer) = this.Subscribe(observer)

    interface IDisposable with
        member this.Dispose() = this.Dispose()