namespace wraikny.Tart.Core.View


open System.Collections.Generic;


/// Interface of updated object with add / remove
[<Interface>]
type IObjectUpdatee<'ViewModel> =
    abstract Update : 'ViewModel -> unit


/// Interface of adding, removing and updating objects
[<Interface>]
type IObjectsUpdater =
    abstract UpdatingEnabled : bool with get, set



/// ViewModel record to updatin objects
type UpdaterViewModel<'ObjectViewModel> =
    {
        nextID : uint32
        objects : Map<uint32, 'ObjectViewModel>
    }


open wraikny.Tart.Helper.Utils


/// Class of adding, removing and updating objects
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
    /// Update objects on ViewModel
    member this.Update(viewModel : UpdaterViewModel<_> option) =
        viewModel |> function
        | Some viewModel ->
            this.AddObjects(&viewModel)
            if (this :> IObjectsUpdater).UpdatingEnabled then
                this.UpdateObjects(&viewModel)
        | None -> ()


    /// Add objects on difference of id in ViewModel
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


    /// Update and remove objects on ViewModel
    member this.UpdateObjects (viewModel : _ inref) =
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