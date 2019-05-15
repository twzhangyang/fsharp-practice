module fsharp.expert.Chapter9.Chapter9Module
open System.Drawing
open System.IO


let a  = seq { 0 .. 2}
let b = seq {-100 .. 100 }
let c = seq {1 .. 2 .. 5}
let range = seq {0 .. 10}
let d = range |> Seq.map ( fun i -> (i, i*i))
let rec allFiles dir =
    Seq.append
        (dir |> System.IO.Directory.GetFiles)
        (dir |> System.IO.Directory.GetDirectories |> Seq.map allFiles |> Seq.concat)
        
        
let squares = seq { for i in 0 .. 10 -> (i, i  * i )}
let e = seq { for (i, iSquared) in squares -> (i, iSquared, i * iSquared) }
let checkerboardCoordinates n =
    seq { for row in 1 .. n do 
            for col in 1 .. n do    
                let sum = row + col
                if sum % 2 = 0 then 
                    yield  (row, col)
    }
    
let fileInfo dir =
    seq {   for file in System.IO.Directory.GetFiles dir do
                let creationTime = System.IO.File.GetCreationTime file
                let lastAccessTime = File.GetLastAccessTime file
                yield  (file, creationTime, lastAccessTime)
    
    }
    
let rec allFiles' dir =
    seq {   for file in Directory.GetFiles dir do 
                yield  file
            for subdir in Directory.GetDirectories dir do 
                yield!  allFiles' subdir
    }
    
let a' = [for i in 0 .. 3 -> (i, i * i)]
let b' = [|for i in 0 .. 3 -> (i, i * i)|]

let people =
    [("Amber", 27, "Design")
     ("Wendy", 35, "Events")
     ("Antonio", 40, "Sales")
     ("Petra", 31, "Design")
     ("Carlos", 34, "Marketing")   
    ]
    
let namesOfPeopleStartingWithA =
    people
    |> Seq.map (fun (name, _age, _dept) -> name)
    |> Seq.filter (fun name -> name.StartsWith "A")
    |> Seq.toList
    
let rand = System.Random()
let randomNumbers = seq { while true do yield rand.Next(100000)}
let firstRenRandomNumbers =
    randomNumbers
        |> Seq.truncate 10
        |> Seq.sort
        |> Seq.toList
        
let triangleNumbers =
    [ 1 .. 10 ]
    |> Seq.collect ( fun i -> [ 1 .. i ] )
    |> Seq.toList
    
let gameBoard =
    [   for i in 0 .. 7 do 
            for j in 0 .. 7 do 
                yield (i, j, rand.Next(10))
    ]
    
let evenPositions =
    gameBoard
        |> Seq.choose ( fun (i, j, v) -> if v % 2 = 0 then Some (i, j) else None)
        |> Seq.toList
        
let firstElementScoringZero =
    gameBoard |> Seq.tryFind (fun (i, j, v) -> v = 0)
 
let firstPositionScoringZero =
    gameBoard |> Seq.tryPick (fun (i, j, v) -> if v = 0 then Some(i, j) else None)
    
let a'' = List.fold (fun acc x -> acc + x) 0 [4; 5; 6]

type Complex(r : float, i : float) =
    static member Polar(mag, phase) = Complex(mag *  cos phase, mag * sin phase)
    member x.Magnitude = sqrt(r * r + i * i)
    member x.Phase = atan2 i r
    member x.RealPart = r
    member x.ImaginaryPart = i
    
let (|Rect|) ( x : Complex) = (x.RealPart, x.ImaginaryPart)
let (|Polar|) (x: Complex) = (x.Magnitude, x.Phase)

let addViaRect a b =
    match a, b with 
    | Rect( ar, ai), Rect(br, bi) -> Complex(ar + br, ai + bi)
    
    
let mulViaRect a b =
    match a, b with 
    | Rect(ar, ai), Rect(br, bi) -> Complex(ar * br - ai * bi, ai * br + bi * ar)
    
let mulViaPolar a b =
    match a, b with 
    | Polar(m, p), Polar(n, q) -> Complex.Polar(m * n, p + q)

let (|Named|Array|Ptr|Param|) (typ: System.Type) =
    if typ.IsGenericType
    then Named(typ.GetGenericTypeDefinition(), typ.GetGenericArguments())
    elif typ.IsGenericParameter then Param(typ.GenericParameterPosition)
    elif not typ.HasElementType then Named(typ,[||])
    elif typ.IsArray then Array(typ.GetElementType(), typ.GetArrayRank())
    elif typ.IsByRef then Ptr(true, typ.GetElementType())
    elif typ.IsPointer then Ptr(false, typ.GetElementType())
    else failwith "MSDN says this can't happen"

let rec formatType typ =
    match typ with 
    | Named (con, [||]) -> sprintf "%s" con.Name
    | Named (con, args) -> sprintf "%s<%s>" con.Name (formatTypes args)
    | Array (arg, rank) -> sprintf "Array(%d, %s)" rank (formatTypes [|arg|])
    | Ptr(true, arg) -> sprintf "%s&" (formatType arg)
    | Ptr(false, arg) -> sprintf "%s" (formatType arg)
and formatTypes typs =
    System.String.Join(",", Array.map formatType typs)
    
    
let rec freeVarsAcc typ acc =
    match typ with 
    | Array (arg, rank) -> freeVarsAcc arg acc
    | Ptr (_, arg) -> freeVarsAcc arg acc
    | Param _ -> (typ :: acc)
    | Named (con, args) -> Array.foldBack freeVarsAcc args acc
    
let freeVars typ = freeVarsAcc typ []

let (|MulThree|_|) inp = if inp % 3 = 0 then Some(inp / 3) else None
let (|MulSeven|_|) inp = if inp % 7 = 0 then Some(inp / 7) else None
let (|MulN|_|) n inp = if inp % n = 0 then Some(inp / n) else None

type Prop = Prop of int
and internal PropRepr =
    | AndRepr of Prop * Prop
    | OrRepr of Prop * Prop 
    | NotRepr of Prop
    | VarRepr of string
    | TrueRepr

//let (|And|Or|Not|Var|True|) prop =
//    match  prop with 
//    | AndRepr (x, y) -> And (x, y)
//    | OrRepr (x, y) -> Or (x, y)
//    | NotRepr x -> Not x
//    | VarRepr v -> Var v
//    | TrueRepr -> True

//let rec showProp precedence prop =
//    let parenIfPrec lim s = if precedence < lim then "(" + s + ")" else s
//    match prop with
//    | Or (p1, p2) -> parenIfPrec 4 (showProp 4 p1 + " || " + showprop 4 p2)
//    | And (p1, p2) -> parenIfPrec 3 (showProp 3 p1 + " && " + showProp 3 p2)
//    | Not p -> parenIfPrec 2 ("not " + showProp 1 p)
//    | Var v -> v
//    | TRUE -> "T"
        
 
    