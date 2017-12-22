namespace fsharp.Classes

type PropertyExample(seed)=
    let mutable myProp=seed


    //public string Seed{get;private set;}
    member this.Seed=seed


    //public string Seed{get;set;}
    member this.MyProp
        with get()=myProp
        and set(value)=myProp<-value


    //public string Seed{get;private set;}
    member this.MyProp2
        with get()=myProp
        and private set(value)=myProp<-value

    //automatic immutable property
    member val ReadOnlyAuto=1

    //automatic mutable property
    member val ReadWriteAuto=1 with get,set




