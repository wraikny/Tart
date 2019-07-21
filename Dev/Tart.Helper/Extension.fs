namespace wraikny.Tart.Helper

open FSharpPlus

module Extension =
    [<CompiledName "FilterMap">]
    let inline filterMap (f : ^a -> ^b option) xs =
        xs
        |>> f
        |> filter Option.isSome
        |>> Option.get