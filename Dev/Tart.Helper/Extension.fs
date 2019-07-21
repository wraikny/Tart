namespace wraikny.Tart.Helper

open FSharpPlus

module Extension =
    let inline filterMap (f : ^a -> ^b option) xs =
        xs
        |>> f
        |> filter Option.isSome
        |>> Option.get