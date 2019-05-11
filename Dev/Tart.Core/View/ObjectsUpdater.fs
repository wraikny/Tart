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
        objects : (uint32 * 'ObjectViewModel) list
    }


open wraikny.Tart.Helper.Utils


/// Class of adding, removing and updating objects
[<Class>]
type ObjectsUpdater<'ViewModel, 'Object, 'ObjectViewModel
    when 'Object :> obj
    and 'Object :> IObjectUpdatee<'ObjectViewModel>
    >(init, add, remove) =
    let objects = new Dictionary<uint32, 'Object>()
    let existFlags = new HashSet<uint32>()

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
            if (this :> IObjectsUpdater).UpdatingEnabled then
                this.UpdateObjects(viewModel)
            else
                this.AddObjects(viewModel)
        | None -> ()


    /// Add objects from ViewModel
    member private this.AddObjects (viewModel) =
        for (id, objectViewModel) in viewModel.objects do
            if not <| objects.ContainsKey(id) then
                let object : 'Object = init()
                object.Update(objectViewModel)
                objects.Add(id, object)
                add(object)


    /// Add, Update, Remove objects from ViewModel
    member private this.UpdateObjects (viewModel) =
        for (id, objectViewModel) in viewModel.objects do
            let (isSuccess, result) = objects.TryGetValue(id)
            if isSuccess then
                result.Update(objectViewModel)
            else
                let object : 'Object = init()
                object.Update(objectViewModel)
                objects.Add(id, object)
                add(object)

            if not <| existFlags.Contains(id) then
                existFlags.Add(id) |> ignore

        let objects' =
            objects
            |> Seq.map(fun x -> (x.Key, x.Value))
            |> Seq.filter(fst >> existFlags.Contains >> not)

        for (id, object) in objects' do
            remove(object)
            objects.Remove(id) |> ignore

        existFlags.Clear()