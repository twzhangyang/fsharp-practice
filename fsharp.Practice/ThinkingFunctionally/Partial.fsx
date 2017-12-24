module partial =
    let add42 = (+) 42
    add42 1
    add42 3

    [1;2;3] |> List.map add42

    let twoIsLessThan = (<) 2
    twoIsLessThan 1
    twoIsLessThan 3

    List.filter
    
    let adderWithPluggableLogger logger x y =
        logger "x" x
        logger "y" y

        let result = x + y
        
        logger "x+y" result
        result

    let consoleLogger argName argValue =
        printfn "%s=%A" argName argValue


    let addWithConsoleLogger = adderWithPluggableLogger consoleLogger
    addWithConsoleLogger 1 2
    addWithConsoleLogger 42 99

    let popupLogger argName argValue =
        let message = sprintf "%s=%A" argName argValue
        //...popup
        message |> ignore
    
    let replace oldStr newStr (s:string) =
        s.Replace(oldValue=oldStr, newValue=newStr)

    let startsWith  lookFor (s:string) =
        s.StartsWith(lookFor)

    let result =
        "hello"
        |> replace "h" "j"
        |> startsWith "j"

    let result2 =
        ["the";"quick";"brown";"fox"]
        |> List.filter (startsWith "f")

    let compositeOp = replace "h" "j" >> startsWith "j"
    let result3 = compositeOp "hello"

    //The reverse pipe function
    let add x y = x + y
    // (1+2) add (3+4) wrong

    1+2 |> add <| 3+4

    let (|>) x f = f x
    let (<|) f x = f x

    let add1 = 1+2 |> add
    let add2 = add1 <| 3+4


    let F1 x y z = x (y z)
    let F2 x y z = y z |> x    
    let F3 x y z = x <| y z 






