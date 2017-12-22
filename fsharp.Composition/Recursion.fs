namespace fsharp.Composition

module Recursion=
    let rec fact x = 
        if x = 1 then 1
        else x * (fact (x - 1))

    printfn "%d" (fact 5)

