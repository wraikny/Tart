namespace wraikny.Tart.Core

open wraikny.Tart.Helper.Utils

[<AbstractClass>]
type Port<'Msg, 'ViewMsg>(messenger) =
    inherit MsgQueue<'ViewMsg>()

    member val Messenger : IMsgQueue<'Msg> = messenger with get

    abstract OnUpdate : 'ViewMsg -> unit

    member __.PushMsg(msg : 'Msg) =
        messenger.Enqueue(msg)
    
    member public this.Update() =
        let rec update() =
            this.TryPopMsg() |> function
            | Some(msg) ->
                this.OnUpdate(msg)
                update()
            | None -> ()

        update()