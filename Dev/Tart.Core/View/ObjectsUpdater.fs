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


[<Interface>]
type IObjectsUpdaterParent<'Object> =
    abstract Create : unit -> 'Object
    abstract Add : 'Object -> unit
    abstract Remove : 'Object -> unit


/// Class of adding, removing and updating objects
[<Class>]
type ObjectsUpdater<'ViewModel, 'Object, 'ObjectViewModel
    when 'Object :> IObjectUpdatee<'ObjectViewModel>
    >(parent) =
    let objects = new Dictionary<uint32, 'Object>()
    let existFlags = new HashSet<uint32>()

    let parent : IObjectsUpdaterParent<'Object> = parent

    interface IObjectsUpdater with
        member val UpdatingEnabled = true with get, set


    /// Update objects on ViewModel
    member this.Update(viewModel : UpdaterViewModel<_> option) =
        viewModel |> function
        | Some viewModel ->
            if (this :> IObjectsUpdater).UpdatingEnabled then
                this.UpdateObjects(viewModel)
            else
                this.AddObjects(viewModel)
        | None -> ()


    /// Add objects on ViewModel
    member private this.AddObjects (viewModel) =
        for (id, objectViewModel) in viewModel.objects do
            if not <| objects.ContainsKey(id) then
                let object : 'Object = parent.Create()
                object.Update(objectViewModel)
                objects.Add(id, object)
                parent.Add(object)


    /// Add, Update, Remove objects on ViewModel
    member private this.UpdateObjects (viewModel) =
        for (id, objectViewModel) in viewModel.objects do
            let (isSuccess, result) = objects.TryGetValue(id)
            if isSuccess then
                result.Update(objectViewModel)
            else
                let object : 'Object = parent.Create()
                object.Update(objectViewModel)
                objects.Add(id, object)
                parent.Add(object)

            if not <| existFlags.Contains(id) then
                existFlags.Add(id) |> ignore

        let objects' =
            objects
            |> Seq.map(fun x -> (x.Key, x.Value))
            |> Seq.filter(fst >> existFlags.Contains >> not)

        for (id, object) in objects' do
            parent.Remove(object)
            objects.Remove(id) |> ignore

        existFlags.Clear()