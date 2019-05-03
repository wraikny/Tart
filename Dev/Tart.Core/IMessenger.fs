namespace wraikny.Tart.Core


/// Telling msg and viewModel, to modelLoop(async).
type IMsgSender<'Msg> =
    /// Add Msg to ConcurrentQueue
    abstract PushMsg : 'Msg -> unit


/// Telling msg and viewModel, between modelLoop(async) and view(mainThread).
[<Interface>]
type IMessenger<'Msg, 'ViewModel> =
    inherit IMsgSender<'Msg>
    /// Sleeping time in every updating
    abstract SleepTime : int with get, set

    /// Thread safe getter of ViewModel
    abstract TryViewModel : 'ViewModel option

    /// Thread safe getter of isRunning flag
    abstract IsRunning : bool with get

    /// Async.Start main loop
    abstract StartAsync : unit -> bool

    /// Async.Start main loop from last model
    abstract ResumeAsync : unit -> bool

    /// Stop asynchronous main loop
    abstract Stop : unit -> unit