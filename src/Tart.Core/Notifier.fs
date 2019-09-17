namespace wraikny.Tart.Core

open System

open wraikny.Tart.Helper.Utils

open FSharpPlus

type Notifier<'T>(tryDequeue : unit -> 'T option) =
    let event = new Event<'T>()

    member __.Pull() =
        tryDequeue()
        |> iter event.Trigger

    member __.PullAll() =
        let rec loop () =
            tryDequeue() |> function
            | Some x ->
                event.Trigger x
                loop()
            | None -> ()

        loop ()

    member __.Subscribe(observer) = event.Publish.Subscribe(observer)
    //member __.Dispose() = observable.Dispose()

    interface IObservable<'T> with
        member this.Subscribe(observer) = this.Subscribe(observer)

    //interface IDisposable with
    //    member this.Dispose() = this.Dispose()