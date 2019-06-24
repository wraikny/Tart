namespace wraikny.Tart.Helper.Collections

open System.Collections
open System.Collections.Generic

type HashMap<'Key, 'T> = class
    val Dict : IReadOnlyDictionary<'Key, 'T>

    new(dict) = { Dict = dict }

    member this.ToSeq() = seq { for x in this.Dict -> (x.Key, x.Value) }

    member private this.GetEnumerator() =
        this.ToSeq().GetEnumerator()

    interface IReadOnlyCollection<'Key * 'T> with
        member this.GetEnumerator() =
            this.GetEnumerator() :> IEnumerator

        member this.GetEnumerator() =
            this.GetEnumerator()

        member this.Count
            with get() = this.Dict.Count
end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashMap =
    open System.Linq

    [<CompiledName "ContainsKey">]
    let containsKey key (hashMap : HashMap<_, _>) =
        hashMap.Dict.ContainsKey(key)

    [<CompiledName "Exists">]
    let exists predicate (hashMap : HashMap<_, _>) =
        hashMap.Dict.Any(fun item -> predicate item.Key item.Value )

    [<CompiledName "OfSeq">]
    let ofSeq seq =
        let dict = new Dictionary<'Key, 'T>()
        for (key, value) in seq do

            if dict.ContainsKey(key) then
                dict.[key] <- value
            else
                dict.Add(key, value)

        new HashMap<'Key, 'T>(dict)

    [<CompiledName "OfList">]
    let ofList list = list |> Seq.ofList |> ofSeq

    [<CompiledName "OfArray">]
    let ofArray array = array |> Seq.ofArray |> ofSeq

    [<CompiledName "ToSeq">]
    let toSeq (hashMap : HashMap<'Key, 'T>) : seq<'Key * 'T> =
        hashMap.ToSeq()

    [<CompiledName "ToList">]
    let toList (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toSeq |> Seq.toList

    [<CompiledName "ToArray">]
    let toArray (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toSeq |> Seq.toArray

    [<CompiledName "TryFind">]
    let tryFind key (hashMap : HashMap<_, _>) =
        hashMap.Dict.TryGetValue(key) |> function
        | true, result -> Some result
        | false, _ -> None

    [<CompiledName "Find">]
    let find key (hashMap : HashMap<_, _>) =
        hashMap.Dict.[key]

    [<CompiledName "Count">]
    let count (hashMap : HashMap<_, _>) = hashMap.Dict.Count