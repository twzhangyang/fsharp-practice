namespace fsharp.DataStructure

module Sequences=
    let ints = [7..13]
    printfn "%A" ints

    let oddValues = [1..2..20]
    printfn "%A" oddValues

    let ints' = seq { 7..13 }
    printfn "%A" ints'

    let list1=[ for x in 7..13 -> x, x*x ]
    printfn "%A" list1

    let yieldedValues = 
        seq {
            yield 3;
            yield 42;
            for i in 1..3 do
                yield i
            yield! [5;7;8]
        }
    printfn "%A" yieldedValues

    let yieldedStrings =
        seq {
            yield "this"
            yield "that"
            }

    printfn "%A" yieldedStrings