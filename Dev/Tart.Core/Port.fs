namespace wraikny.Tart.Core


[<AbstractClass>]
type Port<'Msg, 'ViewMsg>(messenger) =
    inherit MsgQueue<'ViewMsg>()

    let messenger : IMsgSender<'Msg> = messenger

    abstract OnUpdate : 'ViewMsg -> unit

    member __.PushToMessenger(msg : 'Msg) =
        messenger.PushMsg(msg)
    
    member public this.Update() =
        let rec update() =
            this.TryPopMsg() |> function
            | Some(msg) ->
                this.OnUpdate(msg)
                update()
            | None -> ()

        update()