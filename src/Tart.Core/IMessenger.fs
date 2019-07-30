namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils
open System

/// Telling msg and viewModel, between modelLoop(async) and view(mainThread).
[<Interface>]
type IMessenger<'Msg, 'ViewMsg, 'ViewModel> =
    inherit IEnqueue<'Msg>
    inherit IDisposable

    abstract Msg : IObservable<'Msg> with get
    abstract ViewModel : IObservable<'ViewModel> with get
    abstract ViewMsg : IObservable<'ViewMsg> with get

    /// Pull ViewModel and ViewMsgs to update view objects in main thread
    abstract NotifyView : unit -> unit

    /// Sleeping time in every updating
    abstract SleepTime : uint32 with get, set

    /// Thread safe getter of isRunning flag
    abstract IsRunning : bool with get

    /// Async.Start main loop
    abstract StartAsync : unit -> unit

    /// Async.Start main loop from last model
    abstract ResumeAsync : unit -> bool

    /// Stop asynchronous main loop
    abstract Stop : unit -> unit