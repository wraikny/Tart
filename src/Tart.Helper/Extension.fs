namespace wraikny.Tart.Helper

open FSharpPlus

module Extension =
    [<CompiledName "FilterMap">]
    let inline filterMap (f : ^a -> ^b option) xs =
        xs
        |>> f
        |> filter Option.isSome
        |>> Option.get

    [<CompiledName "Pure">]
    let inline pure' (x : ^a) =
        (^a : (static member Return : ^a -> '``M<'a>``) x)

    [<CompiledName "Map2">]
    let inline map2
        (f : 'a -> 'b -> 'c)
        (a : '``Applicative<'a>``)
        (b : '``Applicative<'b>``)
        : '``Applicative<'v>`` = f <!> a <*> b

    [<CompiledName "Indexed">]
    let inline indexed (source : '``Functor<'a>``) : '``Functor<int * 'a>`` =
        let mutable i = -1
        source
        |>> (fun x ->
            i <- i + 1
            (i, x)
        )

    [<CompiledName "IfThen">]
    let inline ifThen cond f = if cond then f else id
    
    [<CompiledName "Increment">]
    let inline increment x = x + one

    [<CompiledName "Decrement">]
    let inline decrement x = x - one