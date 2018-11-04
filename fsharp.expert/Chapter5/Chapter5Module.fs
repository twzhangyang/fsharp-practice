module fsharp.expert.Chapter5.Chapter5Module
open System
open Xunit

type results = string * System.TimeSpan * int * int

type Person =
    {
        Name : string;
        DateOfBirth : DateTime;
    }
    
let a = { Name = "Bill"; DateOfBirth = DateTime(1962, 09, 02) }

type PageStats =
    {
        Site : string
        Time : System.TimeSpan
        Length : int 
        NumWords : int
        NumRefs : int
    }
    
let time f =
    let tm = System.Diagnostics.Stopwatch()
    tm.Start()
    let res = f()
    tm.Stop()
    
    res, tm.Elapsed.TotalMilliseconds
    
let stats site =
    let url = "http://" + site
    let html, t = time (fun () -> url)
    let words = html.Length
    {
        Site = site;
        Time = TimeSpan.FromMilliseconds(t);
        Length = words;
        NumWords = words;
        NumRefs = words;
    }
    
type Point = { X : float; Y : float }

let clone =
    let p1 = { X = 3.0 ; Y = 4.0; }
    let p2 = { p1 with Y = 5.0; }   
    p1, p2

type Route = int 
type Make = string 
type Model = string 

type Transport =
    | Car of Make * Model
    | Bicycle
    | Bus of Route
    
let averageSpeed (tr : Transport) =
    match tr with 
    | Car _ -> 35
    | Bicycle -> 16
    | Bus _ -> 24
    
type Proposition =
    | True
    | And of Proposition * Proposition
    | Or of Proposition * Proposition
    | Not of Proposition
    
let rec eval (p : Proposition) =
    match p with 
    | True -> true
    | And (p1, p2) -> eval p1 && eval p2
    | Or (p1, p2) -> eval p1 || eval p2
    | Not (p1) -> not (eval p1)
    
type 'T list =
    | ([])
    | (::) of 'T * 'T list
    
type Tree<'T> =
    | Tree of 'T * Tree<'T> * Tree<'T>
    | Tip of 'T
    
let rec sizeOfTree tree =
    match tree with 
    | Tree(_, l, r) -> 1 + sizeOfTree l + sizeOfTree r
    | Tip _ -> 1
    
type Point3D = Vector3D of float * float * float
let origin = Vector3D(0., 0., 0.)
let unitX = Vector3D(1.2, 2.0, 3.0)
let length (Vector3D(dx, dy, dz)) = sqrt(dx * dx + dy * dy + dz * dz)

type Node =
    { Name : string ; Links : Link list }
and Link =
    | Dangling
    | Link of Node
    
type Projections<'T, 'U> = ('T -> 'U) * ('U -> 'T)

let mapPair f g (x, y) = (f x, g y)


