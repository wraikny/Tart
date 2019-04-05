namespace wraikny.Tart.Helper.Collections

module Seq =
    let filterMap f =
        Seq.map f
        >> Seq.filter Option.isSome
        >> Seq.map Option.get

    let tryAssoc key =
        Seq.tryFind (fst >> (=) key)
        >> Option.map snd