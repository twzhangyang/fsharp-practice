namespace fsharp.Composition

module LambdaExpression=
    let checkThis (item: 'c) (f: 'c -> bool) : unit =
        if f item then
            printfn "HIT"
        else
            printfn "MISS"

    checkThis 5 (fun x -> x > 3)
    checkThis "hi there" (fun x -> x.Length > 5)

