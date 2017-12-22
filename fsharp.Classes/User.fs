namespace fsharp.Classes

type Gender=
    |Male=0
    |Female=1
    |Unknow=2

type User(name:string,genderNumber:int)=
    do
        if genderNumber>2 then
            failwith "no such gender"
        if genderNumber<0 then
            failwith "no such gender"

    let gender=
        match genderNumber with
        |0->Gender.Male
        |1->Gender.Female
        |_->Gender.Unknow

    let mutable age=10
    let mutable address=""

    new()=User("default",2)


    member x.Name=name
    member x.Gender()=printfn "gender is %A" gender
    member x.Email="xx@xx.com"
    member x.Age with get()=age and set v=age<-v
   
    abstract Greet : unit -> unit
    default x.Greet() = printfn "Hi, I'm %s" x.Name

//     abstract Address:string with get,set  

module Demo=
    let jim=User()
    jim.Gender()

    let tom=User("Tom",1)
    tom.Gender()
    tom.Age<-20
    printfn "tom age is %d " tom.Age



