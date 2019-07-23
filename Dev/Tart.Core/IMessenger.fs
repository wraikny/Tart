namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils
open System

/// Telling msg and viewModel, between modelLoop(async) and view(mainThread).
[<Interface>]
type IMessenger<'Msg, 'ViewMsg, 'ViewModel> =
    inherit IMsgQueue<'Msg>
    inherit IObservable<'Msg>
    inherit IDisposable

    /// Sleeping time in every updating
    abstract SleepTime : uint32 with get, set

    /// Set Port to Messenger
    abstract SetPort : #Port<'Msg, 'ViewMsg> -> unit

    /// Thread safe getter of ViewModel
    abstract TryPopViewModel : 'ViewModel option

    /// Thread safe getter of isRunning flag
    abstract IsRunning : bool with get

    /// Async.Start main loop
    abstract StartAsync : unit -> unit

    /// Async.Start main loop from last model
    abstract ResumeAsync : unit -> bool

    /// Stop asynchronous main loop
    abstract Stop : unit -> unit