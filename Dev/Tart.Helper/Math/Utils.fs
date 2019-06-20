namespace wraikny.Tart.Helper.Math.Utils

open wraikny.Tart.Helper.Math

module BinarySearch =
    let inline binarySearch zero two count predicate current target =
        let rec search count diffSum current target =
            if count <= 0 then diffSum
            else
                let middle = (current + target) / two
                let newDiffSum = diffSum + (middle - current)

                if predicate newDiffSum then
                    search (count - 1) newDiffSum middle target
                else
                    search (count - 1) diffSum current middle

        search count zero current target


    let inline generic count predicate current target =
        let zero = LanguagePrimitives.GenericZero
        let one = LanguagePrimitives.GenericOne
        let two = one + one
        binarySearch zero two
            count predicate current target


    let inline vector count predicate current target =
        let one = Vector.one()
        let two = one + one
        binarySearch (Vector.zero()) two
            count predicate current target