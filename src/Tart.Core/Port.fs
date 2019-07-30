namespace wraikny.Tart.Core

//open wraikny.Tart.Helper.Utils

//[<AbstractClass>]
//type Port<'Msg, 'ViewMsg>(messenger) =
//    inherit MsgQueueSync<'ViewMsg>()

//    member val Messenger : IEnqueue<'Msg> = messenger with get

//    member this.PushMsg(msg : 'Msg) =
//        this.Messenger.Enqueue(msg)