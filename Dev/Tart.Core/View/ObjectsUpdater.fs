namespace wraikny.Tart.Core.View


open System.Collections.Generic;


/// 追加削除の発生するオブジェクトのクラスが実装するインターフェース。
[<Interface>]
type IObjectUpdatee<'ViewModel> =
    abstract Update : 'ViewModel -> unit


/// 追加削除の発生するオブジェクトの更新を行うクラスが実装するインターフェース。 
[<Interface>]
type IObjectsUpdater =
    abstract UpdatingEnabled : bool with get, set



/// 追加削除の発生するオブジェクトの更新を行うためのビューモデル。
type UpdaterViewModel<'ActorViewModel> =
    {
        nextID : uint32
        objects : Map<uint32, 'ActorViewModel>
    }


/// 追加削除の発生するオブジェクトの更新を行うクラス。
[<Class>]
type ObjectsUpdater<'ViewModel, 'Object, 'ObjectViewModel
    when 'Object :> obj
    and 'Object :> IObjectUpdatee<'ObjectViewModel>
    >(init, add, remove) =
    let mutable nextID = 0u
    let objects = new Dictionary<uint32, 'Object>()

    let init = init
    let add = add
    let remove = remove


    interface IObjectsUpdater with
        member val UpdatingEnabled = true with get, set


    /// ビューモデルを元にオブジェクトの更新を行う。
    member this.Update(viewModel : UpdaterViewModel<_> option) =
        viewModel |> function
        | Some viewModel ->
            this.AddObjects(&viewModel)
            if (this :> IObjectsUpdater).UpdatingEnabled then
                this.UpdateActors(&viewModel)
        | None -> ()


    /// ビューモデルを元にidを照合してオブジェクトの追加を行う。
    member this.AddObjects (viewModel : _ inref) =
        let newNextID = viewModel.nextID
        if nextID <> newNextID then
            
            for id in nextID..(newNextID - 1u) do
                viewModel.objects
                |> Map.tryFind id
                |> function
                | None -> ()
                | Some objectViewModel ->
                    let object : 'Object = init()
                    object.Update(objectViewModel)

                    objects.Add(id, object)
                    add(object)

            nextID <- newNextID


    /// ビューモデルを元にオブジェクトの更新と破棄を行う。
    member this.UpdateActors (viewModel : _ inref) =
        let objects' =
            objects
            |> Seq.map(fun x -> (x.Key, x.Value))

        for (id, object) in objects' do
            viewModel.objects
            |> Map.tryFind id
            |> function
            | Some objectViewModel ->
                object.Update(objectViewModel)

            | None ->
                remove(object)
                objects.Remove(id) |> ignore