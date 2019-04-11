namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Wrapper
open wraikny.Tart.Helper.Monad
open System.Collections.Concurrent


[<AbstractClass>]
type Updater<'Msg>() =
    
    let coreMessenger = new CoreMessenger<'Msg>()


    abstract OnUpdate : 'Msg -> unit

    
    member public this.Update() =
        maybe {
            let! msg = coreMessenger.TryPopMsg()
            this.OnUpdate(msg)
        }


    interface IMessageSender<'Msg> with
        member this.PushMsg(msg) = coreMessenger.PushMsg(msg)