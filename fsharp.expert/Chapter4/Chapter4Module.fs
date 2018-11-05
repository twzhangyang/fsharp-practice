module fsharp.expert.Chapter4.Chapter4Module
open System.Collections.Generic
open System.IO
open System
open System.Diagnostics
open System.Net.Http
open System.Runtime.Serialization.Formatters.Binary
open System.Runtime.Versioning

let repeatFetch url n =
    for i = 1 to n do 
        let html = url
        printfn "fetched <<< %s >>> " html
    printfn "Done!"
    
let whileLoops =
    let mutable a = 10
    while a > 0 do 
        printfn "%d" a
        a <- a - 1

let forLoops =       
    for (b, pj) in [("Banana 1 ", false ); ("Banana 2", true )] do 
        if pj then 
            printf "%s is in pyjamas today!" b

type DiscreteEventCoutner =
    {   mutable Total : int;
        mutable Positive : int;
        Name: string
    }
    
let recordEvent (s : DiscreteEventCoutner) isPositive =
    s.Total <- s.Total + 1
    if isPositive then s.Positive <- s.Positive + 1
    
let newCounter nm =
    {Total=0; Positive = 0; Name = nm}
    
let longPageCounter = newCounter "long pages"

let fetchUrl (url:string) =
    let page = url
    recordEvent longPageCounter (page.Length > 1000)
    page
    
let workingWithArray () =
    let arr = [|1.0; 1.0; 1.0|]
    printfn "hello %f" arr.[1]
    arr.[1] <- 3.0
    arr
    
    
let useDictionay =
    let capitals = Dictionary<string, string>(HashIdentity.Structural)
    capitals.["USA"] <- "Washington"
    capitals.["Bangladesh"] <- "Dhaka"
    let a = capitals.ContainsKey("USA")
    a
    
    
let lookupName nm (dict: Dictionary<string, string>) =
    let mutable res = ""
    let foundIt = dict.TryGetValue(nm, &res)
    if foundIt then res
    else failwith "Didn't find "
    

let http (url : string) =
    try
        let req = System.Net.WebRequest.Create(url)
        let resp = req.GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        html
    with 
        | :? System.UriFormatException -> ""
        | :? System.Net.WebException -> ""
        
let raiseException =
    try 
        raise (new System.InvalidOperationException("invalid operation"))
    with err -> printfn "oops, msg = '%s'" err.Message

let httpViaTryFinally (url : string ) =
    let req = System.Net.WebRequest.Create(url)
    let resp = req.GetResponse()
    try 
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        html
    finally 
        resp.Close()
        
exception BlockedURL of string

let http2 url =
    try 
        raise (BlockedURL("http://www.kaos.org"))
    with 
        BlockedURL url -> printfn "blocked! url = '%s'" url
        
    if url = "http://www.kaos.org"
    then raise (BlockedURL(url))
    else http url
   
    
let isWord (words : string list) =
    let wordTable = Set.ofList words
    fun w -> wordTable.Contains(w)
    
type NameLookupService =
    abstract Contains : string -> bool
    
let buildSimpleNameLookup (words : string list) =
    let wordTable = HashSet<_>(words)
    {new NameLookupService with 
        member t.Contains w = wordTable.Contains w
    }
    
let fibFast =
    let t = Dictionary<int, int>()
    let rec fibCached n =
        if t.ContainsKey n then t.[n]
        elif n <= 2 then 1
        else 
            let res = fibCached (n - 1) + fibCached (n - 2)
            t.Add (n, res)
            res
    fun n -> fibCached n
    
let time f =
    let sw = Stopwatch.StartNew()
    let res = f()
    let finish = sw.Stop()
    (res, sw.Elapsed.TotalMilliseconds |> sprintf "%f ms")
    
let memoize (f: 'T -> 'U) =
    let t = Dictionary<'T, 'U>(HashIdentity.Structural)
    fun n -> if t.ContainsKey n then t.[n]
             else 
                let res = f n
                t.Add(n, res)
                res
let rec fibFast' =
    memoize (fun n -> if n <= 2 then 1 else fibFast' (n - 1) + fibFast' (n - 2))
    
type Table<'T, 'U> =
    abstract  Item: 'T -> 'U with get
    abstract  Discard: unit -> unit
    
let memoizeAndPermitDiscard f =
    let lookasideTable = new Dictionary<_, _>(HashIdentity.Structural)
    {new Table<'T, 'U> with 
        member t.Item
            with get(n) =
                if lookasideTable.ContainsKey(n) then 
                    lookasideTable.[n]
                else 
                    let res = f n
                    lookasideTable.Add(n, res)
                    res
        member  t.Discard() =
            lookasideTable.Clear()
    }
    
let rec fibFast'' =
    memoizeAndPermitDiscard (fun n -> 
        printf "computing fibFast %d" n
        if n <= 2 then 1 else fibFast''.[n-1] + fibFast''.[n-2])
        

let useLazy =
    let sixty = lazy (30 + 20)
    sixty.Force()
    
let referenceCell =
    let cell1 = ref 1
    cell1.Value
    cell1 := 3


type 'T reftest =
    { mutable contents : 'T }
    member cell.Value = cell.contents
    
let (!) r = r.contents
let (:=) r v = r.contents <- v
let reftest v = { contents = v }

let linesOfFile =
    seq {
        use reader = new StreamReader(File.OpenRead("test.txt"))
        while  not reader.EndOfStream do 
            yield reader.ReadLine()
    }
    
let prettyPrinting =
    sprintf "result = %A" ([1], [true])
    
let boxTests =
    let a = box 1
    let b = box "abc"
    let c = unbox<string> b
    let d = (unbox b : string)
    d
    
let writeValue outputStream x =
    let formatter = new BinaryFormatter()
    formatter.Serialize(outputStream, x)
    
let readValue inputStream =
    let formatter = new  BinaryFormatter()
    let res = formatter.Deserialize(inputStream)
    
    unbox res
    
let write =
    let addresses = Map.ofList ["Jeff", "123 Main street"; 
                                "Fred", "987 Pine Road"]
    let fsOut = new FileStream("Data.dat", FileMode.Create)
    writeValue fsOut addresses
    fsOut.Close()
    
    let fsIn = new  FileStream("Data.dat", FileMode.Open)
    let res: Map<string, string> = readValue fsIn
    fsIn.Close()
    

type Numeric<'T> =
    {
        Zero : 'T;
        Subtract : ('T -> 'T -> 'T)
        LessThen : ('T -> 'T -> bool)
    }
    
let intOps = { Zero = 0; Subtract = (-); LessThen = (<) }
let bigIntOps = { Zero = 0I; Subtract = (-); LessThen = (<) }
let hcfGeneric (ops: Numeric<'T>) =
    let rec hcf a b =
        if a = ops.Zero then b
        elif  ops.LessThen a b then hcf a (ops.Subtract b a )
        else hcf (ops.Subtract a b ) b
        
    hcf
    
let hcfInt = hcfGeneric intOps
let hcfBigInt = hcfGeneric bigIntOps

let a = hcfInt 18 12

type INumberic<'T> =
    abstract Zero : 'T
    abstract Subtract : 'T * 'T -> 'T
    abstract LessThen : 'T * 'T -> bool
    
let intOps' =
    {new INumberic<int> with 
        member  ops.Zero = 0
        member  ops.Subtract(x, y) = x - y
        member  ops.LessThen(x, y) = x < y}
    
let castingUp =
    let xobj = (1 :> obj)
    let sobj = ("abc" :> obj)
    
    xobj, sobj 
    
let castingDown =
    let boxedObj = box "abc"
    let downcastString = (boxedObj :?> string)
    downcastString     
    
let checkObject (x: obj) =
    match x with 
    | :? string -> printfn "the object is a string"
    | :? int -> printfn "the object is an integer"
    | _ -> printfn "The input is something else"
    
checkObject (box "abc")
    
    
type PingPang = Ping | Pong
let printSecondElements (inp : seq<PingPang * int>) =
    inp
    |> Seq.iter ( fun (x, y) -> printfn "y = %d" y)

    
