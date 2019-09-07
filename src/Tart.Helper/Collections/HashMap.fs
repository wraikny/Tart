namespace wraikny.Tart.Helper.Collections

open System.Collections
open System.Collections.Generic

type HashMap<'Key, 'T when 'Key : equality> private(dict : IReadOnlyDictionary<'Key, 'T>) =
    let count = lazy(dict.Count)
    
    member val internal Dict = dict with get

    static member Empty = new HashMap<'Key, 'T>(new Dictionary<_, _>())

    static member Create(dict : #IDictionary<_, _>) =
        new HashMap<'Key, 'T>(new Dictionary<_, _>(dict :> IDictionary<_, _>))

    static member internal CreateWithoutNew(dict) =
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
            with get() = count.Force()


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashMap =
    open System.Linq

    open FSharpPlus

    [<CompiledName "Empty">]
    let empty<'Key, 'Value when 'Key : equality> =
        HashMap<'Key, 'Value>.Empty

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

        HashMap.CreateWithoutNew(dict)

    [<CompiledName "OfList">]
    let inline ofList list = list |> toSeq |> ofSeq

    [<CompiledName "OfArray">]
    let inline ofArray array = array |> toSeq |> ofSeq

    [<CompiledName "ToSeq">]
    let inline toSeq (hashMap : HashMap<'Key, 'T>) : seq<'Key * 'T> =
        hashMap.ToSeq()

    [<CompiledName "toMap">]
    let inline toMap (hashMap : HashMap<_, _>) =
        hashMap |> toSeq |> Map.ofSeq

    [<CompiledName "ToList">]
    let inline toList (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toSeq |> toList

    [<CompiledName "ToArray">]
    let inline toArray (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toSeq |> toArray

    [<CompiledName "Map">]
    let inline map f hashMap =
        hashMap
        |> toSeq
        |> Seq.map(fun (key, v) -> (key, f key v) )
        |> ofSeq

    [<CompiledName "TryFind">]
    let tryFind key (hashMap : HashMap<_, _>) =
        hashMap.Dict.TryGetValue(key) |> function
        | true, result -> Some result
        | false, _ -> None

    [<CompiledName "Find">]
    let find key (hashMap : HashMap<_, _>) =
        hashMap.Dict.[key]

    [<CompiledName "Count">]
    let inline count (hashMap : HashMap<_, _>) = (hashMap :> IReadOnlyCollection<_>).Count