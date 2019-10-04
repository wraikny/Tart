module wraikny.Tart.Helper.Coroutine


open System.Collections
open System.Linq

let inline sleep n =
    seq { for _ in 1..n -> () }

let inline asParallel (coroutines : seq<seq<unit>>) =
    seq {
        let coroutines =
            (coroutines
            |> Seq.map(fun c -> c.GetEnumerator() :> IEnumerator)
            ).ToList()

        let mutable isContinue = true

        while isContinue do
            isContinue <- false
            for c in coroutines do
                if c.MoveNext() && not isContinue then
                    isContinue <- true
            yield ()
    }