namespace fsharp.Classes

type IAddingService=
    abstract member Add:int->int->int


module ObjectExpression=
    let makeResource name=
        {new System.IDisposable
            with member this.Dispose()=
                    printfn "%s disposed" name}



    let useAndDisposeResource=
        use r1=makeResource "firstResource"
        printfn "using first resource"
        for i in [1..3] do
            let resourceName=sprintf "\tinner reosurce %d" i
            use temp=makeResource resourceName
            printfn "\t do something with %s" resourceName
        use r2=makeResource "second resource"
        printfn "using second resource"
        printfn "done."


    let makeAdder id=
        {new IAddingService with
            member this.Add x y=
                printfn "Adder %i is adding" id
                let result=x+y
                printfn "%i+%i=%i" x y result
                result}

    let testAdders=
        for i in [1..3] do
            let adder=makeAdder i
            let result=adder.Add i i 
            ()

 