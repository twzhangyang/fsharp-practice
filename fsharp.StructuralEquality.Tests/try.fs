namespace tryparse

open System

module _try=
    let (isSucess,value)=Double.TryParse("3.13232")

    Double.TryParse("3.1334") |>(fun result->
        match result with
            |(true,value)->printfn "%f" value
            |(false,_)->printfn "could not parse"
    )