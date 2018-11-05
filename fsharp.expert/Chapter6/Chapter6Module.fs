module fsharp.expert.Chapter6.Chapter6Module
open System.Collections.Generic

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
    
    

    

    