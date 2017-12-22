namespace fsharp.Classes

type Lucy()=
    inherit User("Lucy",1)

//    override x.Address
//        with set v=
//            if v="" then failwith "address must not be empty"
//                else base.Address<-v

        override x.Greet() = printfn "hello everyone, my name is %s " x.Name

