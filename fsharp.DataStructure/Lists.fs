namespace fsharp.DataStructure

module Lists=
    let empty=[]

    let intList=[1;2;3;4]
    printfn "%A" intList.[0]
    printfn "%A" intList

    let addItem xs x=x::xs

    let newIntList=addItem intList 5
    printfn "%A" newIntList
    printfn "%A" newIntList.Head
    printfn "%A" newIntList.Tail
    printfn "%A" newIntList.Tail.Tail.Head

    printfn "%A" (["hi"; "there"] @ ["how";"are";"you"])

    for i in newIntList do
        printfn "%A" i

    let rec listLength (l:'a list)=
        if l.IsEmpty then 0
            else 1+listLength(l.Tail)

    printfn "length is %d" (listLength newIntList)
    
    let rec listLength' l=
        match l with
        |[]->0
        |_::xs->1+ listLength' xs  
        
    printfn "length is %d" (listLength' newIntList)
    
//    let listLength'' l=
//        let rec listLength l length=
//            match l with
//            |[]->0
//            |_::xs->let len=1+length
//                    printfn "len is %d " len
//                    listLength xs len
//        listLength l 0
//
//    printfn "length is %d" (listLength'' newIntList)
       




    

