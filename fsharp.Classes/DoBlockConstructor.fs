namespace fsharp.Classes

type DoExample(seed)=
    let privateValue=seed+1

    do printfn "the private value is now %i" privateValue

type DoPrivateFunctionExample(seed)=
    let privateValue=seed+1

    do printfn "hello world"

    let printPrivateValue=
        do printfn "the private value is now %i" privateValue

    do printPrivateValue


type DoPublicFunctionExample(seed) as this=
    let privateValue=seed+1

    do this.PrintPrivateValue

    member this.PrintPrivateValue=
         printfn "the private value is now %i" privateValue

module Constructor=
    let example=new DoExample(1)

    let example2=DoPrivateFunctionExample(1)

    let example3=DoPublicFunctionExample(1)