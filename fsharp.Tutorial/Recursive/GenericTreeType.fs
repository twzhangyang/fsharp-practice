namespace Practice.Recursive

module GenericTreeType =

    type FileSystemItem =
        | File of FileInfo
        | Directory of DirectoryInfo
        
    and FileInfo = {name:string; fileSize:int}
    and DirectoryInfo = {name:string; dirSize:int;}
    
    
    type Tree<'LeafData, 'INodeData> =
        | LeafNode of 'LeafData
        | InternalNode of 'INodeData * Tree<'LeafData, 'INodeData> seq
        
    type FileSystemItem' = Tree<FileInfo, DirectoryInfo>
    
    let add x y = x + y
    
    let rec cata fLeaf fNode (tree:Tree<'LeafData, 'INodeData>) : 'r =
        let recurse = cata fLeaf fNode
        
        match tree with
        | LeafNode leafInfo ->
            fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            let innerNode = subtrees |> Seq.map recurse
            fNode nodeInfo innerNode
            
    let rec fold fLeaf fNode acc (tree:Tree<'LeafData, 'INodeData>) :'r=
        let recurse = fold fLeaf fNode
        match tree with
        | LeafNode leafinfo ->
            fLeaf acc leafinfo
        | InternalNode (nodeInfo, subtrees) ->
            let localAcc = fNode acc nodeInfo
            let finalAcc = subtrees |> Seq.fold recurse localAcc
            finalAcc
    
    let fromFile (fileInfo:FileInfo) = 
        LeafNode fileInfo 

    let fromDir (dirInfo:DirectoryInfo) subitems = 
        InternalNode (dirInfo,subitems)
       
    let readme = LeafNode {FileInfo.name="readme.txt"; FileInfo.fileSize=1}
    let config = LeafNode {FileInfo.name="config.txt"; FileInfo.fileSize=2}
    let build = LeafNode {FileInfo.name="build.txt"; FileInfo.fileSize=3}
    let src = fromDir {DirectoryInfo.name="src"; DirectoryInfo.dirSize=10} [readme; config; build]
    let bin = fromDir {DirectoryInfo.name="bin"; DirectoryInfo.dirSize=10} []
    let root = fromDir {DirectoryInfo.name="bin"; DirectoryInfo.dirSize=5} [src;bin]
    
    let totalSize fileSystemItem =
        let fFile acc (file:FileInfo) = 
            acc + file.fileSize
        let fDir acc (dir:DirectoryInfo)= 
            acc + dir.dirSize
        fold fFile fDir 0 fileSystemItem 

    readme |> totalSize |> ignore // 1
    src |> totalSize |> ignore    // 16 = 10 + (1 + 2 + 3)
    root |> totalSize |> ignore   // 31 = 5 + 16 + 10
    
    let largestFile fileSystemItem =
        let fFile (largestSoFar:FileInfo option) (file:FileInfo) =
            match largestSoFar with
            | None ->
                Some file
            | Some largestSoFar ->
                if largestSoFar.fileSize > file.fileSize then
                    Some largestSoFar
                else
                    Some file
        
        let fDir (LargestSoFar:FileInfo option) (dir: DirectoryInfo) =
            LargestSoFar
            
        fold fFile fDir None fileSystemItem
        
    
    readme |> largestFile |> ignore
    src |> largestFile |> ignore
    bin |> largestFile |> ignore
    root |> largestFile |> ignore
    
    
    
module GenericTreeTypeTests =
    open GenericTreeType
    open Xunit
    
    [<Fact>]
    let ``hello`` =
        readme |> largestFile
        