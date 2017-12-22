namespace fsharp.Classes

type CustomerName(firstName,middleInitial,lastName)=
    member this.FirstName=firstName
    member this.MiddleName=middleInitial
    member this.LastName=lastName


type CustomerName2(firstName:string,middleInitial:string,lastName:string)=
    member this.FirstName=firstName
    member this.MiddleName=middleInitial
    member this.LastName=lastName


type NonTupledConstructor(x:int,y:int)=
    do printfn "x=%i y=%i" x y
    
    
type TupledConstructor(tuple:int * int)=
    let x,y=tuple
    do printfn "x=%i y=%i" x y    

type PrivateValueExample(seed)=
    let privateValue=seed+1

    let mutable mutableValue=42

    let privateAddToSeed input=seed+input

    member this.AddToSeed x=
        privateAddToSeed x

    member this.SetMutableValue x=
        mutableValue<-x

type MutableConstructorParameter(seed)=
    let mutable mutableSeed=seed
    
    member this.SetSeed x=
        mutableSeed<-x


module Customer=
    let tom=new CustomerName("tom","j","Y")
    let jim=new CustomerName2("jim","j","Y")

    printfn "tom is %A" tom
    printfn "jim is %A" jim

//
//public class CustomerName
//{
//    public CustomerName(string firstName, 
//       string middleInitial, string lastName)
//    {
//        this.FirstName = firstName;
//        this.MiddleInitial = middleInitial;
//        this.LastName = lastName;
//    }
//
//    public string FirstName { get; private set; }
//    public string MiddleInitial { get; private set; }
//    public string LastName { get; private set; }
//}


    let instance=new PrivateValueExample(42)
    printf "%i" (instance.AddToSeed 2)
    instance.SetMutableValue 43



