// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
open System.IO

let add=fun (x:int) (y:int)->x=y

let sum=add 10 20

let addNumber (x:int) (y:int)=x+y

let sum2=addNumber 10 20

let rec length list=
    match list with
    |[]->0
    |x::xs->1+length xs



let minus1 x=x-1
let times2 x=x*2

let result1=minus1 10
let result2=times2 10

let minus1ThenTimes2 x=times2(minus1 x)
let result3=minus1ThenTimes2 10

let minus1ThenTimes2'=minus1>>times2
let result4=minus1ThenTimes2' 10

let times2ThenMinus1 x=minus1(times2 x)
let result5=times2ThenMinus1 10

let times2ThenMinus1'=minus1<<times2
let result6=times2ThenMinus1' 10


try
 failwith "An error message"
with
 | Failure msg->printfn "Failed with %s" msg


//exception CountingException of string*int
//
//try
//    raise (countingException("hello",1))
//with
//    | CountingException(reason,number)->printfn "error is %s" reason


try
    1/0

with
    | :?System.DivideByZeroException as e-> 
    printfn "Don't divided by zero" 
    0

let ReadFile()=
    use reader=new StreamReader("")
    reader.Read()

ReadFile()

let addPair p=
    match p with
    | (0,e)->e
    |(e,0)->e
    |(s,e)->s+e

addPair (1,2)