namespace Catamorphism
module Recursive =
    type FileSystemItem =
        | File of File
        | Directory of Directory
    and File ={name:string; fileSize:int}
    and Directory ={name:string; dirSize:int; subitems:FileSystemItem list}

    let readme = File {name="readme.txt"; fileSize=1}
    let config = File {name="config.xml"; fileSize=2}
    let build  = File {name="build.bat"; fileSize=3}
    let src = Directory {name="src"; dirSize=10; subitems=[readme; config; build]}
    let bin = Directory {name="bin"; dirSize=10; subitems=[]}
    let root = Directory {name="root"; dirSize=5; subitems=[src; bin]}


    let rec cataFS fFile fDir item :'r = 
        let recurse = cataFS fFile fDir 
        match item with
        | File file -> 
            fFile file
        | Directory dir -> 
            let listOfRs = dir.subitems |> List.map recurse 
            fDir (dir.name,dir.dirSize,listOfRs) 

    let totalSize fileSystemItem =
        let fFile (file:File) = 
            file.fileSize
        let fDir (name,size,subsizes) = 
            (List.sum subsizes) + size
        cataFS fFile fDir fileSystemItem

    readme |> totalSize  // 1
    src |> totalSize     // 16 = 10 + (1 + 2 + 3)
    root |> totalSize  

    let largestFile fileSystemItem =

        // helper to provide a default if missing
        let ifNone deflt opt =
            defaultArg opt deflt 

        // helper to get the size of a File option
        let fileSize fileOpt = 
            fileOpt 
            |> Option.map (fun file -> file.fileSize)
            |> ifNone 0

        // handle File case  
        let fFile (file:File) = 
            Some file

        // handle Directory case  
        let fDir (name,size,subfiles) = 
            match subfiles with
            | [] -> 
                None  // empty directory
            | subfiles -> 
                // find the biggest File option using the helper
                subfiles 
                |> List.maxBy fileSize  

        // call the catamorphism
        cataFS fFile fDir fileSystemItem

    readme |> largestFile  
    // Some {name = "readme.txt"; fileSize = 1}

    src |> largestFile     
    // Some {name = "build.bat"; fileSize = 3}

    bin |> largestFile     
    // None

    root |> largestFile    
    // Some {name = "build.bat"; fileSize = 3}

    type Product =
        | Bought of BoughtProduct 
        | Made of MadeProduct 
    and BoughtProduct = {
        name : string 
        weight : int 
        vendor : string option }
    and MadeProduct = {
        name : string 
        weight : int 
        components:Component list }
    and Component = {
        qty : int
        product : Product }

    let label = 
        Bought {name="label"; weight=1; vendor=Some "ACME"}
    let bottle = 
        Bought {name="bottle"; weight=2; vendor=Some "ACME"}
    let formulation = 
        Bought {name="formulation"; weight=3; vendor=None}

    let shampoo = 
        Made {name="shampoo"; weight=10; components=
        [
        {qty=1; product=formulation}
        {qty=1; product=bottle}
        {qty=2; product=label}
        ]}

    let twoPack = 
        Made {name="twoPack"; weight=5; components=
        [
        {qty=2; product=shampoo}
        ]}

    let rec cataProduct fBought fMade product :'r = 
        let recurse = cataProduct fBought fMade 

        // Converts a Component into a (int * 'r) tuple
        let convertComponentToTuple comp =
            (comp.qty,recurse comp.product)

        match product with
        | Bought bought -> 
            fBought bought 
        | Made made -> 
            let componentTuples =  // (int * 'r) list
                made.components 
                |> List.map convertComponentToTuple 
            fMade (made.name,made.weight,componentTuples) 

    let productWeight product =

        // handle Bought case
        let fBought (bought:BoughtProduct) = 
            bought.weight

        // handle Made case
        let fMade (name,weight,componentTuples) = 
            // helper to calculate weight of one component tuple
            let componentWeight (qty,weight) =
                qty * weight
            // add up the weights of all component tuples
            let totalComponentWeight = 
                componentTuples 
                |> List.sumBy componentWeight 
            // and add the weight of the Made case too
            totalComponentWeight + weight

        // call the catamorphism
        cataProduct fBought fMade product


    label |> productWeight    // 1
    shampoo |> productWeight  // 17 = 10 + (2x1 + 1x2 + 1x3)
    twoPack |> productWeight  // 39 = 5 + (2x17)

    type VendorScore = {vendor:string; score:int}
    let vendor vs = vs.vendor
    let score vs = vs.score

    let mostUsedVendor product =

        let fBought (bought:BoughtProduct) = 
            // set score = 1 if there is a vendor
            bought.vendor
            |> Option.map (fun vendor -> {vendor = vendor; score = 1} )
            // => a VendorScore option
            |> Option.toList
            // => a VendorScore list

        let fMade (name,weight,subresults) = 
            // subresults are a list of (qty * VendorScore list)

            // helper to get sum of scores
            let totalScore (vendor,vendorScores) =
                let totalScore = vendorScores |> List.sumBy score
                {vendor=vendor; score=totalScore}

            subresults 
            // => a list of (qty * VendorScore list)
            |> List.collect snd  // ignore qty part of subresult
            // => a list of VendorScore 
            |> List.groupBy vendor 
            // second item is list of VendorScore, reduce to sum
            |> List.map totalScore 
            // => list of VendorScores 

        // call the catamorphism
        cataProduct fBought fMade product
        |> List.sortByDescending score  // find highest score
        // return first, or None if list is empty
        |> List.tryHead
