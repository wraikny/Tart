module wraikny.Tart.Tests.Helper

open Expecto

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math
open FSharpPlus

let isInvalie x =
    System.Double.IsNaN x
    || System.Double.IsInfinity x


[<Tests>]
let vec2Tests =
    testList "Vec2" [
        testProperty "toSeq" <| fun (a : float32 Vec2) ->
            a |> toSeq |> length |> (=) 2

        testProperty "map" <| fun (a : int Vec2) ->
            let f = (+) 1 >> (*) 2 >> (-) 1 >> float32 >> string >> (+) "aaa"
            let b = map f a
            b.x = f a.x && b.y = f a.y
    ]

[<Tests>]
let vectorTests =
    //let inline axes2 (v : _ Vec2) =
    //    let s = Vector.axes() |> List.map((|>) v)
    //    s.[0] = v.x && s.[1] = v.y

    //let inline axes3 (v : _ Vec3) =
    //    let s = Vector.axes() |> List.map((|>) v)
    //    s.[0] = v.x && s.[1] = v.y && s.[2] = v.z

    //let inline axes4 (v : _ Vec4) =
    //    let s = Vector.axes() |> List.map((|>) v)
    //    s.[0] = v.x && s.[1] = v.y && s.[2] = v.z && s.[3] = v.w

    testList "Vector" [
        testList "Applicative" [
            testProperty "int Vec2" <| fun (a : int Vec2) b ->
                let c = (Vec2.init (+) (-)) <*> a <*> b
                (c.x = a.x + b.x) && (c.y = a.y - b.y)

            testProperty "int Vec3" <| fun (a : int Vec3) b ->
                let c = (Vec3.init (+) (-) (*)) <*> a <*> b
                (c.x = a.x + b.x) && (c.y = a.y - b.y) && (c.z = a.z * b.z)

            testProperty "int Vec4" <| fun (a : int Vec4) b ->
                let c = (Vec4.init (+) (-) (*) (*)) <*> a <*> b
                (c.x = a.x + b.x) && (c.y = a.y - b.y) && (c.z = a.z * b.z) && (c.w = a.w * b.w)
        ]

        //testProperty "Axes int Vec2" <| fun (a : int Vec2) -> axes2 a
        //testProperty "Axes int Vec3" <| fun (a : int Vec3) -> axes3 a
        //testProperty "Axes int Vec4" <| fun (a : int Vec4) -> axes4 a

        testProperty "Dot int Vec2" <| fun (a : int Vec2) b ->
            (Vector.dot a b) = (a.x * b.x + a.y * b.y)

        testProperty "Dot int Vec3" <| fun (a : int Vec3) b ->
            (Vector.dot a b) = (a.x * b.x + a.y * b.y + a.z * b.z)

        testProperty "Dot int Vec4" <| fun (a : int Vec4) b ->
            (Vector.dot a b) = (a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w)

        testProperty "XY int Vec2" <| fun (a : int Vec2) ->
            (Vector.xy a) = a

        testProperty "XY int Vec3" <| fun (a : int Vec3) ->
            (Vector.xy a) = Vec2.init a.x a.y

        testProperty "XYZ int Vec3" <| fun (a : int Vec3) ->
            (Vector.xyz a) = a

        testProperty "XYZ int Vec4" <| fun (a : int Vec4) ->
            (Vector.xyz a) = Vec3.init a.x a.y a.z
    ]
