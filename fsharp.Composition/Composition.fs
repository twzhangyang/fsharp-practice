namespace fsharp.Composition

module Composition=
    let add x y=x+y
    let mult x y = x * y

    let add'=fun x y->x+y

    let add'' x=fun y->x+y

    let add10''=add'' 10
    printfn "10+10=%d" (add10'' 10)

    let add10=add 10
    printfn "10+10=%d" (add10 10)

    let mult5 = mult 5
    let calcResult = mult5 (add10 20)
    let calcResult' = 20 |> add10 |> mult5

    let add10mult5 = add10 >> mult5

    let calcResult'' = add10mult5 20

