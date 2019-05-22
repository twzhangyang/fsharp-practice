
module currying =
    let printTwoParameters x y =
        printfn "x=%i y=%i" x y

    let printTwoParameters2 x =
        let subFunction y =
            printfn "x=%i y=%i" x y
        subFunction

    printTwoParameters2 1
    printTwoParameters2 1 2
    (printTwoParameters2 1) 2


    let x = 6
    let y = 99
    let intermediateFn = (+) x
    let result = intermediateFn y

    let result2 = (+) x y

