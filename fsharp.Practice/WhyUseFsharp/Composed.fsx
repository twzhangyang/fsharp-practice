module composed = 
    let add2 x = x + 2
    let mult3 x = x * 3
    let square x = x * x

    [1..10] |> List.map add2 |> printfn "%A"
    [1..10] |> List.map mult3 |> printfn "%A"
    [1..10] |> List.map square |> printfn "%A"

    let add2ThenMult3 = add2 >> mult3
    let mult3ThenAdd2 = mult3 >> add2

    let add2ThenMult32 x = mult3 (add2 x)
    let mult3ThenAdd22 x = add2 (mult3 x)

    let logMsg msg x = printfn "%s%i" msg x
    let logMsgN msg x = printfn "%s%i" msg x


