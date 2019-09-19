namespace wraikny.Tart.Helper

open System.Collections.Generic

open FSharpPlus

/// ViewModel record to updatin objects
type UpdaterViewModel<'ViewModel> = (uint32 * 'ViewModel) list

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
type ObjectsUpdater<'Object, 'ObjectViewModel
    when 'Object :> IUpdatee<'ObjectViewModel>
    >(parent) =
    let objects = Dictionary<uint32, 'Object>()
    let existFlags = HashSet<uint32>()

    let parent : ObjectsParent<'Object, 'ObjectViewModel> = parent

    let objectPooling = Stack<'Object>()

    let mutable updatingOption = UpdatingWithPooling

    member __.UpdatingOption
        with get() = updatingOption
        and  set(x) =
            updatingOption <- x
            if x <> UpdatingWithPooling then
                objectPooling.Clear()


    /// Update objects on ViewModel
    member this.Update(viewModel : UpdaterViewModel<_>) =
        if updatingOption.EnabledUpdating then
            this.UpdateObjects(viewModel)
        else
            this.AddObjects(viewModel)


    member private this.Create() =
        if (updatingOption = UpdatingWithPooling) && objectPooling.Count > 0 then
            objectPooling.Pop()
        else
            parent.create()


    member this.Remove(id : uint32) =
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
        |> filter(existFlags.Contains >> not)
        |> toList // mutability
        |> iter this.Remove

        existFlags.Clear()