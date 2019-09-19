﻿namespace wraikny.Tart.Math.Algorithm

open wraikny.Tart.Math
open System.Linq


type UnionFold(num) =
    let par = (seq { for i in 0..num -> i }).ToList()
    let rank = (seq { for _ in 0..num -> 0 }).ToList()

    member private this.Root (node : int) =
        if node < 0 then
            invalidArg "node" "node must not be less than"

        if par.[node] = node then
            node
        else
            let r = this.Root(par.[node])
            par.[node] <- r
            r


    member this.InSameSet(edge : Edge<_, _>) =
        this.Root(edge.node1.label) = this.Root(edge.node2.label)


    member this.Unite(edge : Edge<_, _>) =
        let x = this.Root(edge.node1.label)
        let y = this.Root(edge.node2.label)

        if x = y then ()
        elif rank.[x] < rank.[y] then
            par.[x] <- y
        else
            par.[y] <- x
            if rank.[x] = rank.[y] then
                rank.[x] <- rank.[x] + 1