namespace wraikny.Tart.Helper.Utils

open System.Collections
open System.Collections.Generic

type HashMap<'Key, 'T> = class
    val Dict : IReadOnlyDictionary<'Key, 'T>

    new(dict) = { Dict = dict }

    member this.ToSeq() = seq { for x in this.Dict -> (x.Key, x.Value) }

    member private this.GetEnumerator() =
        this.ToSeq().GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator

    interface IEnumerable<'Key * 'T> with
        member this.GetEnumerator() = this.GetEnumerator()

    interface IReadOnlyCollection<'Key * 'T> with
        member this.Count with get() = this.Dict.Count
end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashMap =
    open System.Linq

    let containsKey key (hashMap : HashMap<_, _>) =
        hashMap.Dict.ContainsKey(key)

    let exists predicate (hashMap : HashMap<_, _>) =
        hashMap.Dict.Any(fun item -> predicate item.Key item.Value )

    let toSeq (hashMap : HashMap<'Key, 'T>) : seq<'Key * 'T> =
        hashMap.ToSeq()

    let ofSeq seq =
        let dict = new Dictionary<'Key, 'T>()
        for (key, value) in seq do

            if dict.ContainsKey(key) then
                dict.[key] <- value
            else
                dict.Add(key, value)

        new HashMap<'Key, 'T>(dict)

    let ofList list = list |> Seq.ofList |> ofSeq

    let ofArray array = array |> Seq.ofArray |> ofSeq

    let toList (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toSeq |> Seq.toList

    let toArray (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toSeq |> Seq.toArray

    let tryFind key (hashMap : HashMap<_, _>) =
        hashMap.Dict.TryGetValue(key) |> function
        | true, result -> Some result
        | false, _ -> None

    let find key (hashMap : HashMap<_, _>) =
        hashMap.Dict.[key]

    let count (hashMap : HashMap<_, _>) = hashMap.Dict.Count