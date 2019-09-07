namespace wraikny.Tart.Helper.Math

// open wraikny.Tart.Helper

open FSharpPlus

module Utils =
    let inline inCollision (aLeft : 'a, aRight : 'a) (bLeft, bRight) =
        not (aRight < bLeft || bRight < aLeft)


module Angle =
    [<CompiledName "Pi">]
    let pi = float32 System.Math.PI

    [<CompiledName "DegreeToRadian">]
    let inline degreeToRadian degree =
        degree * pi / 180.0f

    [<CompiledName "RadianToDegree">]
    let inline radianToDegree radian =
        radian * 180.0f / pi


module BinarySearch =
    let inline binarySearch count predicate (current : 'a) (target : 'a) : 'a =
        let two : 'a = one + one

        let rec search count diffSum current target =
            if count <= 0 then diffSum
            else
                let middle = (current + target) / two
                let newDiffSum = diffSum + (middle - current)

                if predicate newDiffSum then
                    search (count - 1) newDiffSum middle target
                else
                    search (count - 1) diffSum current middle

        search count (zero : 'a) current target