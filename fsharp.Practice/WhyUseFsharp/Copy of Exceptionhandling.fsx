module exceptions = 
    open System.IO
    open System.Security

    type Result<'a,'b> = 
        | Success of 'a
        | Failure of 'b

    type FileErrorReason = 
        | FileNotFound of string
        | UnauthorizedAccess of string * System.Exception

    let performActionOnFile action filePath = 
        try
            use sr = new StreamReader(filePath:string)
            let result = action sr
            sr.Close()
            Success (result)
        with
            | :? FileNotFoundException as ex
                -> Failure (FileNotFound filePath)
            | :? SecurityException as ex
                -> Failure (UnauthorizedAccess (filePath,ex))
    
    let middleLayerDo action filePath = 
        let fileResult = performActionOnFile action filePath
        fileResult

    let topLayerDo action filePath = 
        let filerResult = middleLayerDo action filePath
        filerResult

    let printFirstLineOfFile filePath = 
        let fileResult = topLayerDo (fun fs->fs.ReadLine()) filePath

        match fileResult with
        | Success result->
            printfn "first line is : '%s'" result
        | Failure reason->
            match reason with
            | FileNotFound file -> printfn "file not found %s" file
            | UnauthorizedAccess (file,_) -> printfn "you do not have access to the file: %s" file
    