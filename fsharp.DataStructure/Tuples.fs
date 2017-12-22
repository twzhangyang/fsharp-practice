namespace fsharp.DataStructure

module Tuples=
    let t1 = 12, 5, 7
    let v1, v2, _ = t1

    let t2 = "hi", true

    printfn "%A" (fst t2)
    printfn "%A" (snd t2) 

