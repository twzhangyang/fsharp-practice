module Calculator=
    let add num1 num2=
        num1+num2
    
    let multiply num1 num2=
        num1*num2


module Calculator2=
    let square num=
        num*num
        

 // function
 module example =
    open System.Runtime.InteropServices.ComTypes

    let myInt = 5
    let myFloat = 3.14
    let myString = "hello"

    // list
    let twoToFive = [2;3;4;5]
    let oneToFive = 1 :: twoToFive
    let zeroToFive = [0;1] @ twoToFive  

    // function
    let square x = x * x
    square 3 |> ignore
    
    let add x y = x + y
    add 2 3 |> ignore
    
    let evens list =
        let isEven x = x % 2 = 0
        List.filter isEven list
    evens oneToFive |> ignore
    
    let sumOfSquaresTo100 = 
        List.sum (List.map square [1..100])
    
    let sumOfSquaresTo100piped = 
        [1..100] |> List.map square |> List.sum
    
    let sumOfSquaresTo100withFun = 
        [1..100] |> List.map (fun x->x*x) |> List.sum

    //Pattern Matching
    let simplePatternMatch = 
        let x = "a"
        match x with
            | "a" -> printfn "x is a"
            | "b" -> printfn "x is b"
            | _ -> printfn "x is something else"

    let validValue = Some(99)
    let invalidValue = None

    let optionPatternMatch input =
        match input with
            | Some i -> printfn "input is an int=%d" i
            | None -> printfn "input is missing"
    
    //Complex data type
    // tuple
    let twoTuple = 1,2
    let threeTuple = "a",2,true
    // record type
    type Person = {
        First:string;
        last:string
        }
    let person1 = {
        First="john";
        last="Doe"
        }

    // Union type
    type Temp = 
        | DegreeC of float
        | DegreeF of float
    let temp = DegreeF 98.6

    type Employee = 
        | Worker of Person
        | Manager of Employee list
    let jdoe = {First="John";last="Doe"}
    let worker = Worker jdoe
    let manager = Manager [worker]

    printfn "Printing an int %i, a float %f, a bool %b" 1 2.0 true
    printfn "A string %s, and something generic %A" "hello" [1;2;3;4]

    printfn "twoTuple=%A,\nPerson=%A,\nTemp=%A,\nEmploye=%A" twoTuple person1 temp worker

    type IntAndBool = {intPart:int; boolPart:bool}
    let x = {intPart=1; boolPart=false}
    type IntOrBool = 
        | IntChoice of int
        | BoolChoice of bool
    let y = IntChoice 42
    let z = BoolChoice true

    // if then else
    let ifelse booleanExpression = 
        match booleanExpression with
        | true -> true
        | false -> false
    
    let switch value = 
        match value with
        | 1 -> 1
        | 2 -> 2
        | 3 -> 3
    
    type Shape =
        | Circle of radius:int
        | Rectangle of height:int * width:int
        | Point of x:int * y:int
        | Polygon of pointList:(int*int) list

    let draw shape =
        match shape with
        | Circle radius ->
            printfn "The circle has a radius of %d" radius
        | Rectangle (height,width) ->
            printfn "The rectangle is %d high by %d wide" height width
        | Polygon points ->
            printfn "The polygon is made of these points %A" points
        | _ -> printfn "I don't recognize this shape"

    let circle  = Circle(10)
    let circle2 = Circle 10
    let point = Point(2,3)
    let rectangle = Rectangle(10,12)
    let polygon = Polygon([(1,2);(2,3)])


module comparing = 
    let square x = x * x
    let sumOfSquares n = 
        [1..n] |> List.map square |> List.sum

    let sum = sumOfSquares 100
    printfn "sum is %A" sum

    let sum2 = 
        [1..100] 
        |> List.map (fun c -> c * c)
        |> List.sum
    printfn "sum2 is %A" sum2
 
 
 module sorting =
    let rec quicksort list =
        match list with
        | [] -> []
        | firstElem::otherElements ->
            let smallerElements =
                otherElements
                |> List.filter (fun c -> c < firstElem)
                |> quicksort
            let largerElements =
                otherElements
                |> List.filter (fun c -> c >= firstElem)
                |> quicksort
            List.concat [smallerElements; [firstElem]; largerElements]
    printfn "%A" (quicksort [1;5;23;18;9;1;3])


    let rec quicksort2 = function
        | [] -> []
        | first::rest ->
            let smaller,larger = List.partition ((>=) first) rest
            List.concat [quicksort2 smaller; [first]; quicksort2 larger]

    printfn "%A" (quicksort2 [1;5;23;18;9;1;3])


 module boilerplate = 
    let product n =
        let initialValue = 1
        let action productSoFar x = productSoFar * x
        [1..n] |> List.fold action initialValue
    printfn "The product n is %A" (product 16)

    let sumOfOdds n =
        let initialValue = 0
        let action sumSoFar x = if x%2=0 then sumSoFar else sumSoFar+x
        [1..n] |> List.fold action initialValue
    printfn "The sumOfOdds n is %A" (sumOfOdds 10)

    let alternatingSum n =
        let initialValue = (true,0)
        let action (isNeg,sumSoFar) x = if isNeg then (false,sumSoFar-x)
                                        else (true,sumSoFar+x)
        [1..n] |> List.fold action initialValue |> snd
    printfn "the alternatingSum is %A" (alternatingSum 10)



    



