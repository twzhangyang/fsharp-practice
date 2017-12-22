open System
let a="hello, world"

printfn "%s" a

let g x f=f x

let add a b=
    let value=1
    let value2=2
    let sub=
        a-b
    2
    a+b

add 1 2





   















let (|>) x f=f x

let users=[1;2;3;4]
let firtUser=users |> List.map (fun x->x*x)

let square x=x*x
let toStr (x:int)=x.ToString()
let rev (x:string)=String(Array.rev (x.ToCharArray()))

let result=rev (toStr (square 512))







