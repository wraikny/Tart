namespace wraikny.Tart.Helper.Monad

[<AutoOpen>]
module Maybe =
    type MaybeBuilder() =
        let bind x k =
            x |> function
            | Some value -> k value
            | None -> None

        let mreturn = Some

        member __.Bind(x, k) = bind x k

        member __.Return(x) = mreturn x

        member __.ReturnFrom(x) = x

        member __.Delay(f) = f()

        member __.Combine(a, b) =
            if Option.isSome a then a else b ()

        member __.Zero() = None

        member __.For(inp,f) =
            seq {for a in inp -> f a}

        member __.Yield(x) = mreturn(x)

        member __.YieldFrom(x) = x

    let maybe = new MaybeBuilder()