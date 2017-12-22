namespace fsharp.Classes

[<AbstractClass>]
type BaseClass(param1)=
    member this.Param1=param1

    abstract member Add:int->int->int
    abstract member P1:float
    abstract member Area:float with get,set

    default this.Add x y=x+y
    default this.P1=3.14

type DerivedClass(param1,param2)=
    inherit BaseClass(param1)
    let mutable area=1.0

    override this.Area 
        with get()=3.13
        and set(value)=area<-value

[<AbstractClass>]
type Animal()=
    abstract member MakeNoise:unit->unit


type Dog()=
    inherit Animal()
    override this.MakeNoise()=printfn "woof"

type Vehicle()=
    abstract member TopSpeed:unit->int
    default this.TopSpeed()=60

type Rocket()=
    inherit Vehicle()
    override this.TopSpeed()=base.TopSpeed()*10


module InheritDemo=
    let vehicle=new Vehicle()
    printfn "vehicle.TopSpped=%i" <| vehicle.TopSpeed()


//public class MyBaseClass
//{
//    public MyBaseClass(int param1)
//    {
//        this.Param1 = param1;
//    }
//    public int Param1 { get; private set; }
//}
//
//public class MyDerivedClass: MyBaseClass
//{
//    public MyDerivedClass(int param1,int param2): base(param1)
//    {
//        this.Param2 = param2;
//    }
//    public int Param2 { get; private set; }
//}