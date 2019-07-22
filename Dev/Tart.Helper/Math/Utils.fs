namespace wraikny.Tart.Helper.Math.Utils

open wraikny.Tart.Helper.Math

open FSharpPlus

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