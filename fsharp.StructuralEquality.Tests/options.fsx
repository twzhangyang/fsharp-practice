
open System

let firstOdd xs=
    List.tryPick(fun x->if x%2=1 then Some x else None) xs


let firstOdd'=
    List.tryPick (fun x->if x%2=1 then Some x else None)

firstOdd [2;4;6]
firstOdd' [2;4;5;6;7]

let print o=
    match o with
    | Some v-> sprintf "%A" v
    | None ->"nothing"



let div num1 num2=
    num1/num2

div 15 3
div 15 0

let SafeDiv num den=
    if den=0. then
        Choice1Of2 "dived by zero is undefined"
    else
        Choice2Of2 (num/den)

SafeDiv 15. 3.
SafeDiv 15. 0.
