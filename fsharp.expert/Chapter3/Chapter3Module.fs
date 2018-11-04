module fsharp.expert.Chapter3.Chapter3Module
open System
open System.Drawing

let round x =
    if x >= 100 then 100
    elif x < 0 then 0
    else  x
    
let round' x =
    match x with 
    | _ when x >= 100 -> 100
    | _ when x < 0 -> 0
    | _ -> 0
    
let rec factorial (n:int) = if n <= 1 then 1 else n * factorial (n - 1);

let rec length l =
    match l with 
    | [] -> 0
    | h :: t -> 1 + length t  
    
let list' =
    let x = [1..99]
    let y = [for x in 1..99 -> x * x]
    let oddPrimes = [3;5;7]
    let morePrimes = [13;17]
    let primes = 2 :: (oddPrimes @ morePrimes)
    primes 

let list'' =
    let a = List.head [5;4;3]
    let b = List.tail [5;4;3]
    let c = List.map ( fun x -> x * x) [1;2;3]
    let d = List.filter (fun x -> x % 3 = 0) [2;3;5;7;9]
    d
    
    
let option' =
    let people =[("Adam", None), ("Eve", None), ("Cain", Some("Adam", "Eve")), ("Abel", Some("hello"))]
    people
    
let isLikelySecretAgent url agent =
    match (url, agent) with 
    | "http://www.control.org", 99 -> true 
    | "http://www.control.org", 86 -> true 
    | _ -> true 
    
let printFirst xs =
    match xs with 
    | h :: t -> printfn "The first item in the list is %A " h
    | [] -> printfn "No items in the list"
    
let highLow a b =
    match (a, b) with 
    | ("lo", lo), ("hi", hi) -> (lo, hi)
    | ("hi", hi), ("lo", lo) -> (lo, hi)
    | _ -> failwith "unexpetec input"
    
highLow ("hi", 300) ("lo", 100) |> ignore

let sign x =
    match x with 
    | _ when x < 0 -> -1
    | _ when x > 0 -> 1
    | _ -> 0
    
let getValue a =
    match a with 
    | (("lo" | "low" ), v) -> v
    | ("hi", v) | ("high", v) -> v
    | _ -> failwith "expected a both a high and low value"

let remap (r1:RectangleF) (r2:RectangleF) =
    let scalex = r2.Width / r1.Width
    let scaley = r2.Height / r1.Height
    let mapx x = r2.Left + (x - r1.Left) * scalex
    let mapy y = r2.Top + (y - r1.Top) * scaley
    let mapp (p:PointF) = PointF(mapx p.X, mapy p.Y)
    mapp
    
let ``remap tests`` =
    let rect1 = RectangleF(100.0f, 100.0f, 100.0f, 100.0f)
    let rect2 = RectangleF(50.0f, 50.0f, 200.0f, 200.0f)
    
    let mapp = remap rect1 rect2
    mapp
    
let ``iterating`` =
    let sites = ["http://www.bing.com"; "http://www.google.com"]
    sites |> List.iter ( fun site -> printfn "%s, length = %d" site site.Length)
    
let time f =
    let start = DateTime.Now
    let res = f()
    let finish = DateTime.Now
    (res, finish - start)
    
