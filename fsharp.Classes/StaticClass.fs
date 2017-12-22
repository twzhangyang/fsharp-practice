namespace fsharp.Classes

type StaticExample()=
    member this.InstanceValue=1
    static member StaticValue=2


type StaticConstructor()=
    static let rand=new System.Random()
    static do
        printfn "Class initialization"

    member this.GetRand()=rand.Next()

type AccessibilityExample() = 
    member this.PublicValue = 1
    member private this.PrivateValue = 2
    member internal this.InternalValue = 3

type AccessibilityExample2() = 
    let mutable privateValue = 42
    member this.PrivateSetProperty
        with get() = 
            privateValue 
        and private set(value) = 
            privateValue <- value


module StaticTest=
    let instance=new StaticExample()
    printf "%i" instance.InstanceValue
    printf "%i" StaticExample.StaticValue


