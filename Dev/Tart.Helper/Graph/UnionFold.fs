namespace wraikny.Tart.Helper.Graph

open System.Linq

[<Class>]
type UnionFold(num) =
    let par =
        (seq {
            for i in 0..num -> i
        }).ToList()

    let rank =
        (seq {
            for _ in 0..num -> 0
        }).ToList()

    member private this.Root (node : int) =
        if par.[node] = node then
            node
        else
            let r = this.Root(par.[node])
            par.[node] <- r
            r


    member this.InSameSet(node1, node2) =
        this.Root(node1) = this.Root(node2)


    member this.Unite(node1, node2) =
        let x = this.Root(node1)
        let y = this.Root(node2)

        if x = y then ()
        elif rank.[x] < rank.[y] then
            par.[x] <- y
        else
            par.[y] <- x
            if rank.[x] = rank.[y] then
                rank.[x] <- rank.[x] + 1