namespace fsharp.Classes

type MethodExample()=

    member this.AddOne x=x+1

    member this.AddTwo x=
        this.AddOne x |> this.AddOne

    member this.Pi=3.14

type TupleAndCurriedMethodExample()=
    member this.CurriedAdd x y=
        x+y

    member this.TupleAdd(x,y)=
        x+y

type LetBoundFunctions()=
    let listReduce reducer list=
        list |>List.reduce reducer

    let reduceWithSum sum element=
        sum + element
    
    let sum list=
        list |>listReduce reduceWithSum
       
    member this.Sum=sum
        

type MethodWithRecursiveExample()=
    member this.Fib x=
        match x with
        | 0|1->1
        |_->this.Fib(x-1)+this.Fib(x-2)

type MethodWithAnnotation()=
    member this.AddThree (x:int):int=
        x+3

module MethodClass=
    let me=new MethodExample()
    printfn "%i" <| me.AddOne 42
    printfn "%i" <| me.AddTwo 42
    printfn "%f" <| me.Pi

    let tc=new TupleAndCurriedMethodExample()
    printfn "%i" <| tc.CurriedAdd 1 2
    printfn "%i" <| tc.TupleAdd(1,2)

    let addOne=tc.CurriedAdd 1
    printfn "%i" <| addOne 99

    let lbf=new LetBoundFunctions()
    printfn "Sum is %i" <| lbf.Sum [1..10]

    let me2=new MethodWithRecursiveExample()
    printfn "%i" <| me2.Fib 10
