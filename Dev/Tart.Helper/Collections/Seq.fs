namespace wraikny.Tart.Helper.Collections

module Seq =
    [<CompiledName "FilterMap">]
    let inline filterMap f : #seq<'a> -> seq<'b> =
        Seq.map f
        >> Seq.filter Option.isSome
        >> Seq.map Option.get

    [<CompiledName "TryAssoc">]
    let inline tryAssoc key : #seq<'a * 'b> -> 'b option =
        Seq.tryFind (fst >> (=) key)
        >> Option.map snd