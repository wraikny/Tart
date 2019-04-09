namespace wraikny.Tart.Helper.Monad

[<AutoOpen>]
module Either =
    type EitherBulder() =
        let bind x k =
            x |> function
            | Ok value -> k value
            | Error e -> Result.Error e

        let mreturn = Ok

        member __.Bind(x, k) = bind x k

        member __.Return(x) = mreturn x

        member __.ReturnFrom(x) = x

        member __.Delay(f) = f()

        member __.Combine(a, b) =
            a |> function
            | Ok _ -> a
            | Error _ -> b ()

        // member __.Zero() = None

        member __.For(inp,f) =
            seq {for a in inp -> f a}

        member __.Yield(x) = mreturn(x)

        member __.YieldFrom(x) = x

    let either = new EitherBulder()