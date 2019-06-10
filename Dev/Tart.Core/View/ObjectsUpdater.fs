namespace wraikny.Tart.Core.View


open System.Collections.Generic;


/// Interface of adding, removing and updating objects
[<Interface>]
type IObjectsUpdater =
    abstract EnabledUpdating : bool with get, set
    abstract EnabledPooling : bool with get, set



/// ViewModel record to updatin objects
type UpdaterViewModel<'ObjectViewModel> =
    {
        objects : (uint32 * 'ObjectViewModel) list
    }


[<Interface>]
type IObjectsUpdaterParent<'Object, 'ObjectViewModel> =
    abstract Create : unit -> 'Object
    abstract Add : 'Object -> unit
    abstract Update : 'Object * 'ObjectViewModel -> unit
    abstract Remove : 'Object -> unit
    abstract Dispose : 'Object -> unit


/// Class of adding, removing and updating objects
[<Class>]
type ObjectsUpdater<'ViewModel, 'Object, 'ObjectViewModel>(parent) =
    let objects = new Dictionary<uint32, 'Object>()
    let existFlags = new HashSet<uint32>()

    let parent : IObjectsUpdaterParent<'Object, 'ObjectViewModel> = parent

    let objectPooling = new Stack<'Object>()

    let mutable enabledUpdating = true
    let mutable enabledPooling = true

    interface IObjectsUpdater with
        member this.EnabledUpdating
            with get() = enabledUpdating
            and set(value) =
                if not value then
                    objectPooling.Clear()
                    enabledPooling <- false

                enabledUpdating <- value

        member __.EnabledPooling
            with get() = enabledPooling
            and set(value) =
                if not value then
                    objectPooling.Clear()

                enabledPooling <- value


    /// Update objects on ViewModel
    member this.Update(viewModel : UpdaterViewModel<_> option) =
        viewModel |> function
        | Some viewModel ->
            if (this :> IObjectsUpdater).EnabledUpdating then
                this.UpdateObjects(viewModel)
            else
                this.AddObjects(viewModel)
        | None -> ()


    member private this.Create() =
        if (this :> IObjectsUpdater).EnabledPooling then
            if objectPooling.Count > 0 then
                objectPooling.Pop()
            else
                parent.Create()
        else
            parent.Create()


    member private this.Remove(id : uint32) =
        let object = objects.Item(id)
        parent.Remove(object)
        objects.Remove(id) |> ignore
        if (this :> IObjectsUpdater).EnabledPooling then
            objectPooling.Push(object)
        else
            parent.Dispose(object)


    /// Add objects on ViewModel
    member private this.AddObjects (viewModel) =
        for (id, objectViewModel) in viewModel.objects do
            if not <| objects.ContainsKey(id) then
                let object : 'Object = this.Create()
                parent.Update(object, objectViewModel)
                objects.Add(id, object)
                parent.Add(object)


    /// Add, Update, Remove objects on ViewModel
    member private this.UpdateObjects (viewModel) =
        for (id, objectViewModel) in viewModel.objects do
            objects.TryGetValue(id) |> function
            | true, result ->
                parent.Update(result, objectViewModel)
            | false, _ ->
                let object : 'Object = parent.Create()
                parent.Update(object, objectViewModel)
                objects.Add(id, object)
                parent.Add(object)

            if not <| existFlags.Contains(id) then
                existFlags.Add(id) |> ignore

        let removedObjectIDs =
            objects
            |> Seq.map(fun x -> x.Key)
            |> Seq.filter(existFlags.Contains >> not)

        for id in removedObjectIDs do
            this.Remove(id)

        existFlags.Clear()