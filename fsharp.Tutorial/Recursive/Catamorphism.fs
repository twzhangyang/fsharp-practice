namespace Practice.Recursive

module Catamorphism =

    type FileSystemItem =
        | File of File
        | Directory of Directory

    and File = { name : string; fileSize : int }
    and Directory = { name : string; dirSize : int; subitems : FileSystemItem list }

    let readme = File { name = "readme.txt"; fileSize = 1 }
    let config = File { name = "config.xml"; fileSize = 2 }
    let build = File { name = "build.bat"; fileSize = 3 }
    let src = Directory { name = "src"; dirSize = 10; subitems = [ readme; config; build ] }
    let bin = Directory { name = "bin"; dirSize = 10; subitems = [] }
    let root = Directory { name = "root"; dirSize = 5; subitems = [ src; bin ] }

    let rec cataFS fFile fDir item =
        let recurse = cataFS fFile fDir
        match item with
        | File file ->
            fFile file
            
        | Directory dir ->
            let innerRec = dir.subitems |> List.map recurse 
            fDir dir.name dir.dirSize innerRec 
            
    let totalSize fileSystemItem =
        let fFile (file:File) = file.fileSize
        let fDir name size subsizes = size + (subsizes |> List.sum) 
        
        cataFS fFile fDir fileSystemItem
        
        
    readme |> totalSize |> ignore
    src |> totalSize |> ignore
    root |> totalSize |> ignore

    
    let largestFile fileSystemItem =
        let fFile (file:File) = Some file
        let fileSize file =
            match file with
            | None -> 0
            | Some file -> file.fileSize
            
        let ifNone default' fileOpt =
            defaultArg fileOpt default'
            
        let fileSize' file =
            file |> Option.map (fun file -> file.fileSize)
                 |> ifNone 0
           
            
        let fDir name size items =
            match items with
            | [] -> None
            | items ->
                items |> List.maxBy fileSize
                
        cataFS fFile fDir fileSystemItem
        
    readme |> largestFile |> ignore
    src |> largestFile |> ignore
    bin |> largestFile |> ignore
    root |> largestFile |> ignore
        
module CatamorphismProduct =
    type Product =
        | Bought of BoughtProduct
        | Made of MadeProduct
    and BoughtProduct = {
        name: string
        weight: int
        vendor: string option
    }
    and MadeProduct = {
        name: string
        weight: int
        components: Component list
    }
    and Component = {
        qty : int
        product: Product
    }
    
    let label =  Bought {name="label"; weight=1; vendor= Some "ACME"}
    let bottle = Bought {name="bottle"; weight=2; vendor= Some "ACME"}
    let formulation = Bought {name="formulation"; weight=3; vendor = None}
    let shampoo = Made { name = "shampoo"; weight=10; components =
        [
            {qty=1; product=formulation}
            {qty=1; product=bottle}
            {qty=2; product=label}
        ]
    }
    let twoPack = Made {name="twoPack"; weight=5; components=
        [
            {qty=2; product=shampoo}
        ]}
    
    let rec cataProduct fBought fMade product =
        let recurse = cataProduct fBought fMade
        match product with
        | Bought bought ->
            fBought bought 
        | Made made ->
            let innerRec = made.components |> List.map (fun com -> (recurse com.product), com.qty)
            fMade made.name made.weight innerRec
            
    let productWeight product =
        let fBought (bought:BoughtProduct) = bought.weight
        let fMade name weight components = weight + (components |> List.sumBy (fun (a, b) -> a * b))
        
        cataProduct fBought fMade product
            
    label |> productWeight |> ignore
    shampoo |> productWeight |> ignore
    twoPack |> productWeight |> ignore
    
    
    type Vendor = { name:string; used:int }
    
    
//    let mostUsedVendor product =
//        let fBought (bought:BoughtProduct) =
//            bought.vendor |> Option.map (fun s -> { name = s; used = 1 })
//                          |> Option.toList  
//        
//        
//        let totalUsed (vendors:Vendor list) =
//            let total = vendors |> List.sumBy (fun v -> v.used)
//            total
//        
//        let fMade name weight components: Vendor * int list =
//            components |> List.collect fst
//                       |> List.groupBy (fun (v:Vendor) -> v.name)
//                       |> List.map (fun (name, (vendors: Vendor list)) -> { name; used = (totalUsed vendors) })
//                       
//        cataProduct fBought fMade product
//    

