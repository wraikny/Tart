namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Helper.Monad
open System.Collections.Concurrent


[<AbstractClass>]
type Port<'ViewMsg>() =
    let coreMessenger = new CoreMessenger<'ViewMsg>()

    abstract OnUpdate : 'ViewMsg -> unit
    
    member public this.Pop() =
        let rec update() =
            coreMessenger.TryPopMsg() |> function
            | Some(msg) ->
                this.OnUpdate(msg)
                update()
            | None -> ()

        update()


    interface IMsgSender<'ViewMsg> with
        member this.PushMsg(msg) = coreMessenger.PushMsg(msg)