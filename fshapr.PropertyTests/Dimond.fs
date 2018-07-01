module FSharp.PropertyTests.Dimond
open System

let make letter =
    let makeLine letterCount letter =
        let padding = String(' ', letterCount - 1)
        sprintf "%s%c%c%s" padding letter letter padding

    let letters = ['A' .. letter]
    letters @
    (letters |> List.rev |> List.tail)
    |> List.map (makeLine letters.Length)
    |> List.reduce (fun x y -> sprintf "%s%s%s" x Environment.NewLine y)


