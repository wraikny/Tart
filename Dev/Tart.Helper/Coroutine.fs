module wraikny.Tart.Helper.Coroutine


open System.Collections
open System.Linq

/// 指定したフレーム数待機するコルーチンを生成する。
let waitFrames n = seq{ for _ in 1..n -> () }


/// 複数のコルーチンを同時に実行するコルーチンを生成する。
let asParallel (coroutines : #seq<seq<unit>>) =
    seq {
        let coroutines =
            (coroutines
            |> Seq.map(fun c -> c.GetEnumerator() :> IEnumerator)
            ).ToList()

        let mutable isContinue = true

        while isContinue do
            isContinue <- false
            for c in coroutines do
                if c.MoveNext() && isContinue = false then
                    isContinue <- true
            yield ()
    }