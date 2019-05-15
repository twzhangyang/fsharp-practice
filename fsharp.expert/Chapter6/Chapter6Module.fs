
module fsharp.expert.Chapter6.Chapter6Module
open System
open System.Collections.Generic
open System.Drawing
open System.IO
open System.Runtime.CompilerServices
open System.Text

type Vector2D =
    { DX : float; DY: float}
    member v.Length = sqrt(v.DX * v.DX + v.DY * v.DY)
    member v.Scale(k) = { DX = k * v.DX; DY = k * v.DY }
    member v.ShiftX x = { v with DX = v.DX + x }
    member v.ShiftY y = { v with DY = v.DY + y }
    member v.ShiftXY (x, y) = { DX = v.DX + x; DY = v.DY + y }
    static member Zero = { DX = 0.0; DY = 0.0 }
    static member ConstX dx = { DX = dx; DY = 0.0 }
    static member ConstY dy = { DX = 0.0; DY = dy }
    
let v = { DX = 3.0; DY = 4.0 }
let a = v.Length
let b = v.Scale(2.0).Length
let c = Vector2D.ConstX(3.0)

type Vector2D'(dx: float, dy : float) =
    let len = sqrt(dx * dx + dy * dy)
    member v.DX = dx    
    member v.DY = dy
    member v.Length = len
    member v.Scale k = Vector2D'(k * dx, k * dy)
    member v.ShiftX(x) = Vector2D'(dx + x, dy)
    member v.ShiftY(y) = Vector2D'(dx, dy + y)
    static member Zero = Vector2D'(0.0, 0.0)
    static  member ConstX (dx) = Vector2D'( dx = dx, dy = 0.0 )
    static member ConstY dy = Vector2D'( dx = 0.0, dy = dy )
    
let v' = Vector2D'(dx = 3.0, dy = 4.0)
let a' = v'.Length
let b' = v'.Scale(2.0).Length
let c' = Vector2D'.ConstX(3.0)

type UnitVector2D(dx, dy) =
    let tolerance = 0.000001
    let length = sqrt(dx * dx + dy * dy)
    do if abs (length - 1.0) >= tolerance then failwith "not a unit vector"
    
    member v.DX = dx
    member v.DY = dy
    new() = UnitVector2D(1.0, 0.0)
    

type Vector2D''(dx: float, dy: float) =
    static let zero = Vector2D''(0.0, 0.0)
    static let onex = Vector2D''(1.0, 0.0)
    static let oney = Vector2D''(0.0, 1.0)
    
    static member Zero = zero
    static member OneX = onex
    static member OneY = oney
    
type SparseVector(items: seq<int * float>) =
    let elems = new SortedDictionary<_, _>()
    do items |> Seq.iter (fun (k, v) -> elems.Add(k,v))
    
    member t.Item
        with get(id) =
            if elems.ContainsKey(id) then elems.[id]
            else 0.0
            
let vector =  SparseVector([2, 343.0])
let a'' = vector.[1]

type Vector2DWithOperators(dx: float, dy: float) =
    member  x.DX = dx
    member  x.DY = dy
    
    static member (+) (v1: Vector2DWithOperators, v2: Vector2DWithOperators) =
        Vector2DWithOperators(v1.DX + v2.DX, v1.DY + v2.DY)
        
    static member (-) (v1: Vector2DWithOperators, v2: Vector2DWithOperators) =
        Vector2DWithOperators(v1.DX - v2.DX, v1.DY - v2.DY)
        
let v1 = Vector2DWithOperators(3.0, 4.0)
let vv1 = v1 + v1

type LabelInfo(?text: string, ?font : string) =
    let text = defaultArg text ""
    let font = match font with 
                | None -> "font"
                | Some v -> v
    
    member x.Text = text
    member x.Font = font
    
    static member Create(?text, ?font) = new LabelInfo(?text=text, ?font=font)

type MutableVector2D(dx :float, dy: float) =
    let mutable currDX = dx
    let mutable currDY = dy
    
    member vec.DX with get() = currDX and set v = currDX <- v
    member vec.DY with get() = currDY and set v = currDY <- v
    
    member vec.Length
        with get() = sqrt(currDX * currDX + currDY * currDY)
        and set len =
            let theta = vec.Angle
            currDX <- cos theta * len
            currDY <- sin theta * len
            
    member vec.Angle
        with get() = atan2 currDY currDX
        and set theta =
            let len = vec.Length
            currDX <- cos theta * len
            currDY <- sin theta * len
            
type IntegerMatrix(rows: int, cols: int) =
    let elems = Array2D.zeroCreate<int> rows cols
    
    member t.Item 
        with get(id1, id2) = elems.[id1, id2]
        and set (id1, id2) v = elems.[id1, id2] <- v
        
let m = IntegerMatrix(2,3)
m.[1,2] <- 2

type  LabelInfoWithPropertySetting() =
    member  v.Name = "lable"
    member val Text = "" with get, set
    
type IShape =
    abstract Contains : Point -> bool
    abstract BoundingBox : Rectangle

//Object interface type using object expression
let circle ( center : Point, radius : int ) =
    {   new IShape with 
            member x.Contains(p : Point) =
                let dx = float32 ( p.X - center.X)
                let dy = float32 ( p.Y - center.Y)
                sqrt(dx * dx + dy * dy) <= float32 radius
            
            member x.BoundingBox =
                Rectangle(center.X - radius, center.Y - radius, 2 * radius + 1, 2 * radius + 1)
    }    

let bigCircle = circle(Point(0,0), 100)
let c'' = bigCircle.Contains(Point(70, 70))


//
type MutableCircle() =
    member val Center = Point(x = 0, y = 0) with get, set
    member val Raius = 10 with get, set
    member c.Peerimeter = 2.0 * Math.PI * float c.Raius
    
    interface IShape with 
        member c.Contains ( p: Point ) =
            let dx = float32 ( p.X - c.Center.X)
            let dy = float32 ( p.Y - c.Center.Y)
            sqrt(dx * dx + dy * dy) <= float32 c.Raius
        
        member c.BoundingBox =
            Rectangle( c.Center.X - c.Raius, c.Center.Y - c.Raius, 2 * c.Raius + 1, 2 * c.Raius + 1 )
            
type ITextOutputSink =
    abstract WriteChar : char -> unit
    abstract  WriteString : string -> unit
    
let simpleOutputSink writeCharFunction =
    {   new ITextOutputSink with 
            member x.WriteChar(c) = writeCharFunction c
            member x.WriteString(s) = s |> String.iter x.WriteChar
    }
    
let stringbuilderOutputSink (buf : StringBuilder) =
    simpleOutputSink ( fun c -> buf.Append(c) |> ignore)
    
type CountingOutputSink(writeCharFunction : char -> unit) =
    let mutable count = 0
    interface  ITextOutputSink with 
        member x.WriteChar(c) = 
            count <- count + 1
            writeCharFunction(c)
        member x.WriteString(s) =
            s |> String.iter ( x :> ITextOutputSink).WriteChar
    member x.Count = count
        
[<AbstractClass>]
type TextOutputSink() =
    abstract WriteChar : char -> unit
    abstract WriteString : string -> unit
    default x.WriteString s = s |> String.iter x.WriteChar 
    
type HtmlWrite() =
    let mutable count = 0
    let sink =
        {   new TextOutputSink() with 
                member x.WriteChar c =
                    count <- count + 1
                    System.Console.Write c
        }
    member x.CharCount = count
    member x.OpenTag(tagName) = sink.WriteString(sprintf "<%s>" tagName)
    member x.CloseTag(tagName) = sink.WriteString(sprintf "</%s>" tagName)
    member x.WriteString(s) = sink.WriteString(s)
    
type CountingOutputSinkByInheritance() =
    inherit TextOutputSink()
    
    let mutable count = 0
    member sink.Count = count
    default sink.WriteChar c =
        count <- count - 1
        System.Console.Write(c)

[<AbstractClass>]        
type ByteOutputSink() =
    inherit TextOutputSink()
    abstract WriteByte : byte -> unit
    abstract WriteBytes : byte[] -> unit
    default sink.WriteChar c = sink.WriteBytes(Encoding.UTF8.GetBytes [|c|])
    override sink.WriteString s = sink.WriteBytes(Encoding.UTF8.GetBytes s)
    default sink.WriteBytes b = b |> Array.iter sink.WriteByte

let myWriteStirngToFile() = 
    use outp = File.CreateText("playlist.txt")
    outp.WriteLine("Enchanted")
    outp.WriteLine("Putyour records on")
    
let myWriteStringToFile' () =
    let outp = File.CreateText("playlist.text")
    try 
        outp.WriteLine("Enchanted")
        outp.WriteLine("put your records on")    
    finally 
        (outp :> System.IDisposable).Dispose()
        
// Extending existing types and moudles        
type System.Int32 with 
    member i.IsPrime = true 
    member i.Hello = "hello"
    
[<Extension>]    
type Int32Extensions() =
    [<Extension>]
    static member IsPrime(i: int) = true 
    
    [<Extension>]
    static member Hello(i:int) = "hello"
    
    
module List =
    let rec pairwise l =
        match l with 
        | [] | [_] -> []
        | h1 :: ((h2 :: _) as t) -> (h1, h2) :: pairwise t
        
let pairwise() =
    let a = List.pairwise [1; 2; 3; 4;]
    a

[<Class>]    
type Vector2D'''(dx : float, dy : float) =
    let len = sqrt(dx * dx + dy * dy)
    member v.DX = dx
    member v.DY = dy
    member v.Length = len
    

type IShape' =
    interface 
        abstract Contains : Point -> bool
        abstract BoundingBox : Rectangle
    end 
    
type IShape'' =
    abstract Contains : Point -> bool
    abstract  BoundingBox : Rectangle
    
[<Struct>]    
type Vector2DStruct(dx : float, dy : float) =
    member v.DX = dx
    member v.DY = dy
    member v.Length = sqrt ( dx * dx + dy * dy)
    
//Working with null values
let switchOnType ( a : obj) =
    match a with 
    | null -> printf "null!"
    | :? System.Exception as e -> printf "An exception: %s" e.Message
    | :? System.Int32 as I -> printf "An integer: %d" I
    | :? System.DateTime as d ->printf "A date/time : %O" d
    | _ -> printf "Some other kind of object"
    

    

            

