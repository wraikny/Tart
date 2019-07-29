module wraikny.Tart.Tests.Helper

open Expecto

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math
open FSharpPlus

[<Tests>]
let vec2Tests =
    testList "Vec2" [
        testCase "Add" <| fun _ ->
            let v0 : int Vec2 = zero
            let v1 : int Vec2 = one
            Expect.isTrue (v0 + v1 = v1) "zero + one = one"

        testCase "Mul" <| fun _ ->
            let v0 : int Vec2 = zero
            let v1 : int Vec2 = one
            Expect.isTrue (v0 * v1 = v0) "zero + one = zero"
    ]