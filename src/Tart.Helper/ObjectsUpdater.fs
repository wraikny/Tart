namespace wraikny.Tart.Helper

open System.Collections.Generic

/// ViewModel record to updatin objects
type UpdaterViewModel<'Key, 'ViewModel> = ('Key * 'ViewModel) list

[<Struct>]
type UpdatingOption =
    | Adding
    | Updating
    | UpdatingWithPooling
with
    member x.EnabledUpdating = x |> function
        | Updating | UpdatingWithPooling -> true
        | Adding -> false


type IUpdatee<'a> =
    abstract Update : 'a -> unit


type ObjectsParent<'Object, 'ObjectViewModel
    when 'Object :> IUpdatee<'ObjectViewModel>
    > = {
    create : unit -> 'Object
    add : 'Object -> unit
    remove : 'Object -> unit
    dispose : 'Object -> unit
}


/// Class of adding, removing and updating objects
type ObjectsUpdater<'Key, 'Object, 'ObjectViewModel
    when 'Object :> IUpdatee<'ObjectViewModel>
    and  'Key : equality
    >(parent) =
    let objects = Dictionary<'Key, 'Object>()
    let existFlags = HashSet<'Key>()

    let parent : ObjectsParent<'Object, 'ObjectViewModel> = parent

    let objectPooling = ObjectsPool<'Object>(parent.create)

    let mutable updatingOption = UpdatingWithPooling

    member __.UpdatingOption
        with get() = updatingOption
        and  set(x) =
            updatingOption <- x
            if x <> UpdatingWithPooling then
                objectPooling.Clear()


    /// Update objects on ViewModel
    member this.Update(viewModel : UpdaterViewModel<'Key, _>) =
        if updatingOption.EnabledUpdating then
            this.UpdateObjects(viewModel)
        else
            this.AddObjects(viewModel)


    member private this.Create() =
        if (updatingOption = UpdatingWithPooling) then
            objectPooling.Pop()
        else
            parent.create()


    member this.Remove(id : 'Key) =
        let object = objects.Item(id)
        objects.Remove(id) |> ignore
        if (updatingOption = UpdatingWithPooling) then
            parent.remove(object)
            objectPooling.Push(object)
        else
            parent.dispose(object)


    /// Add objects on ViewModel
    member private this.AddObjects (viewModel) =
        for (id, objectViewModel) in viewModel do
            if not <| objects.ContainsKey(id) then
                let object : 'Object = this.Create()
                object.Update(objectViewModel)
                objects.Add(id, object)
                parent.add(object)


    /// Add, Update, Remove objects on ViewModel
    member private this.UpdateObjects (viewModel) =
        for (id, objectViewModel) in viewModel do
            objects.TryGetValue(id) |> function
            | true, result ->
                result.Update(objectViewModel)
            | false, _ ->
                let object : 'Object = this.Create()
                object.Update(objectViewModel)
                objects.Add(id, object)
                parent.add(object)

            existFlags.Add(id) |> ignore


        objects
        |> Seq.map(fun x -> x.Key)
        |> Seq.filter(existFlags.Contains >> not)
        |> Seq.toList // mutability
        |> Seq.iter this.Remove

        existFlags.Clear()