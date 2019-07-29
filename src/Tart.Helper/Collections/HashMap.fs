namespace wraikny.Tart.Helper.Collections

open System.Collections
open System.Collections.Generic

type HashMap<'Key, 'T when 'Key : equality> = class
    val Dict : IReadOnlyDictionary<'Key, 'T>

    private new(dict) = { Dict = dict }

    static member Create(dict : #IDictionary<_, _>) =
        new HashMap<'Key, 'T>(new Dictionary<_, _>(dict :> IDictionary<_, _>))

    static member CreateWithoutNew(dict) =
        new HashMap<'Key, 'T>(dict)

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

    open FSharpPlus

    [<CompiledName "ContainsKey">]
    let inline containsKey key (hashMap : HashMap<_, _>) =
        hashMap.Dict.ContainsKey(key)

    [<CompiledName "Exists">]
    let inline exists predicate (hashMap : HashMap<_, _>) =
        hashMap.Dict.Any(fun item -> predicate item.Key item.Value )

    [<CompiledName "OfSeq">]
    let inline ofSeq seq =
        let dict = new Dictionary<'Key, 'T>()
        for (key, value) in seq do

            if dict.ContainsKey(key) then
                dict.[key] <- value
            else
                dict.Add(key, value)

        HashMap.CreateWithoutNew(dict)

    [<CompiledName "OfList">]
    let inline ofList list = list |> toSeq |> ofSeq

    [<CompiledName "OfArray">]
    let inline ofArray array = array |> toSeq |> ofSeq

    [<CompiledName "ToSeq">]
    let inline toSeq (hashMap : HashMap<'Key, 'T>) : seq<'Key * 'T> =
        hashMap.ToSeq()

    [<CompiledName "ToList">]
    let inline toList (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toSeq |> toList

    [<CompiledName "ToArray">]
    let inline toArray (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toSeq |> toArray

    [<CompiledName "TryFind">]
    let inline tryFind key (hashMap : HashMap<_, _>) =
        hashMap.Dict.TryGetValue(key) |> function
        | true, result -> Some result
        | false, _ -> None

    [<CompiledName "Find">]
    let inline find key (hashMap : HashMap<_, _>) =
        hashMap.Dict.[key]

    [<CompiledName "Count">]
    let inline count (hashMap : HashMap<_, _>) = hashMap.Dict.Count