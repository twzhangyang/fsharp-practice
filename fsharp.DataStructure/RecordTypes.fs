namespace fsharp.DataStructure

module RecordTypes=
    type Rectangle={Width:float;Height:float}

    let rec1={Width=10.2;Height=2.6}

    type Circle = 
        { mutable Radius : float }
        member x.RadiusSquare with get() = x.Radius * x.Radius
        member x.CalcArea() = System.Math.PI * x.RadiusSquare

    let c1 = {Radius = 3.3}
    c1.Radius <- 5.4
    printfn "Radius square is %A" c1.RadiusSquare

    type Ellipse = 
        { RadiusX: float; RadiusY: float }
        member x.GrowX dx = { x with RadiusX = x.RadiusX + dx }
        member x.GrowY dy = { x with RadiusY = x.RadiusY + dy }

