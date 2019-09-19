namespace wraikny.Tart.Math

open System.Collections
open System.Collections.Generic
open System.Linq
open FSharpPlus

type HashMap<'Key, 'T when 'Key : equality> private(dict : IReadOnlyDictionary<'Key, 'T>) =
    let count = lazy(dict.Count)
    
    member val internal Dict = dict with get

    static member internal Empty = new HashMap<'Key, 'T>(new Dictionary<_, _>())

    static member inline internal Create(dict : #IDictionary<_, _>) =
        new HashMap<'Key, 'T>(new Dictionary<_, _>(dict :> IDictionary<_, _>))

    static member inline internal CreateWithoutNew(dict) =
        new HashMap<'Key, 'T>(dict)

    static member Map(x : HashMap<'Key, 'T>, f) =
        let d = Dictionary<_, _>()
        for i in x.Dict do
            d.Add(i.Key, f i.Key i.Value)
        
        HashMap(d)

    member this.ToSeq() = seq { for x in this.Dict -> (x.Key, x.Value) }

    member this.GetEnumerator() =
        this.ToSeq().GetEnumerator()

    member __.Count with get() = count.Force()

    interface IReadOnlyCollection<'Key * 'T> with
        member this.GetEnumerator() =
            this.GetEnumerator() :> IEnumerator

        member this.GetEnumerator() =
            this.GetEnumerator()

        member this.Count with get() = this.Count


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashMap =
    let empty<'Key, 'Value when 'Key : equality> =
        HashMap<'Key, 'Value>.Empty

    let containsKey key (hashMap : HashMap<_, _>) =
        hashMap.Dict.ContainsKey(key)

    let exists predicate (hashMap : HashMap<_, _>) =
        hashMap.Dict.Any(fun item -> predicate item.Key item.Value )

    let ofSeq seq =
        let dict = new Dictionary<'Key, 'T>()
        for (key, value) in seq do

            if dict.ContainsKey(key) then
                dict.[key] <- value
            else
                dict.Add(key, value)

        HashMap.CreateWithoutNew(dict)

    let inline ofList list = list |> toSeq |> ofSeq

    let inline ofArray array = array |> toSeq |> ofSeq

    let inline toSeq (hashMap : HashMap<'Key, 'T>) : seq<'Key * 'T> =
        toSeq hashMap

    let inline toMap (hashMap : HashMap<_, _>) =
        hashMap |> toSeq |> Map.ofSeq

    let inline toList (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toList

    let inline toArray (hashMap : HashMap<'Key, 'T>) =
        hashMap |> toArray

    let inline map f hashMap =
        hashMap
        |> FSharpPlus.Operators.map f

    let tryFind key (hashMap : HashMap<_, _>) =
        hashMap.Dict.TryGetValue(key) |> function
        | true, result -> Some result
        | false, _ -> None

    let find key (hashMap : HashMap<_, _>) =
        hashMap.Dict.[key]

    let inline count (hashMap : HashMap<_, _>) = (hashMap :> IReadOnlyCollection<_>).Count