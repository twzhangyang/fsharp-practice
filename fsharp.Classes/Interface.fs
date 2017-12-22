namespace fsharp.Classes

type MyInterface=
    abstract member Add:int->int->int

    abstract member Pi:float

    abstract member Area:float with get,set


[<AbstractClass>]
type AbstractBaseClass()=
     abstract member Add:int->int->int
     
     abstract member Pi:float
     
     abstract member Area:float with get,set
     
     
 type IAddingService=
    abstract member Add:int->int->int
    
    
 type MyAddingService()=
    interface IAddingService with
        member this.Add x y=
            x+y
    interface System.IDisposable with
        member this.Dispose()=
            printfn "disposed"
            
            
            
module InterfaceDemo=
    let service=new MyAddingService()
    //error
//    let result=service.Add 1 2      
    let service2=service :>IAddingService
    let result=service2.Add 1 2

    let testAddingService (adder:IAddingService)=
        printfn "1+2=%i" <| adder.Add 1 2

    let service3=new MyAddingService()
    let result3=testAddingService service3

    let testDispose=
        use service4=new MyAddingService()
        printfn "testing"
