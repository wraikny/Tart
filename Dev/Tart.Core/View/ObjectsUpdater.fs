namespace wraikny.Tart.Core.View

open wraikny.Tart.Helper.Utils
open System.Collections.Generic
open System

open FSharpPlus

/// ViewModel record to updatin objects
type UpdaterViewModel<'ViewModel> = (uint32 * 'ViewModel) list

/// Interface of adding, removing and updating objects
[<Interface>]
type IUpdater<'ViewModel> =
    // inherit IObserver<UpdaterViewModel<'ViewModel>>
    abstract EnabledUpdating : bool with get, set
    abstract EnabledPooling : bool with get, set



[<Struct>]
type ObjectsParent<'Object, 'ObjectViewModel
    when 'Object :> IUpdatee<'ObjectViewModel>
    > = {
    create : unit -> 'Object
    add : 'Object -> unit
    remove : 'Object -> unit
    dispose : 'Object -> unit
}


/// Class of adding, removing and updating objects
[<Class>]
type ObjectsUpdater<'Object, 'ObjectViewModel
    when 'Object :> IUpdatee<'ObjectViewModel>
    >(parent) =
    let objects = new Dictionary<uint32, 'Object>()
    let existFlags = new HashSet<uint32>()

    let parent : ObjectsParent<'Object, 'ObjectViewModel> = parent

    let objectPooling = new Stack<'Object>()

    let mutable enabledUpdating = true
    let mutable enabledPooling = true

    interface IUpdater<'ObjectViewModel> with
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
    member this.Update(viewModel : UpdaterViewModel<_>) =
        if (this :> IUpdater<_>).EnabledUpdating then
            this.UpdateObjects(viewModel)
        else
            this.AddObjects(viewModel)


    member private this.Create() =
        if (this :> IUpdater<_>).EnabledPooling then
            try objectPooling.Pop()
            with | :? System.InvalidOperationException -> parent.create()
        else
            parent.create()


    member private this.Remove(id : uint32) =
        let object = objects.Item(id)
        objects.Remove(id) |> ignore
        if (this :> IUpdater<_>).EnabledPooling then
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
                let object : 'Object = parent.create()
                object.Update(objectViewModel)
                objects.Add(id, object)
                parent.add(object)

            existFlags.Add(id) |> ignore

        let removedObjectIDs =
            objects
            |> Seq.map(fun x -> x.Key)
            |> filter(existFlags.Contains >> not)
            |> toList

        for id in removedObjectIDs do
            this.Remove(id)

        existFlags.Clear()