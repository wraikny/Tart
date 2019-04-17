namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Wrapper
open wraikny.Tart.Helper.Monad
open System.Collections.Concurrent


[<AbstractClass>]
type Updater<'Msg>() =
    
    let coreMessenger = new CoreMessenger<'Msg>()


    abstract OnUpdate : 'Msg -> unit

    
    member public this.Update() =
        let rec update() =
            coreMessenger.TryPopMsg() |> function
            | Some(msg) ->
                this.OnUpdate(msg)
                update()
            | None -> ()

        update()


    interface IMsgSender<'Msg> with
        member this.PushMsg(msg) = coreMessenger.PushMsg(msg)