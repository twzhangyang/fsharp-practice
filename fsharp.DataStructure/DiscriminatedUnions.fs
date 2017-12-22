namespace fsharp.DataStructure

module DiscriminatedUnions=
    type Gender = 
    | Male = 0
    | Female = 1

    type Product = 
    | Fruit of int
    | OfficeSupplies of string*int
    | Sports of string*string*int

    let p1 = Product.Fruit(10)
    let p2=Product.OfficeSupplies("Paper",1000)
    let p3=Product.Sports("Basketball","ball",10)

    type Trade=
    | Incoming of Product*decimal
    | Outcoming of Product*decimal

    type Trade with
        member x.IsIncomingTrade()=
            match x with
            | Incoming(_,_)->true
            |_->false

    let t1=Incoming(p1,2.3m)
    printfn "%A" (t1.IsIncomingTrade())

