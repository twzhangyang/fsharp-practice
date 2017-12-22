namespace fsharp.Classes

type MultipleConstructors(param1,param2)=
    do printfn "Param1=%i Param2=%i" param1 param2

    new(param1)=
        MultipleConstructors(param1,-1)

    new()=
        printfn "Constructing..."
        MultipleConstructors(13,7)

