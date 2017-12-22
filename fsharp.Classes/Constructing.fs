namespace fsharp.Classes

type MyClass(param1:int,param2:string)=
    member this.Two=2
    member this.Square x=x*x



module ConstructingDemo=
    let myInstance=new MyClass(1,"hello")

    let myInstance2=MyClass(2,"hello")
    let point=System.Drawing.Point(1,2)


    let sr1 = System.IO.StringReader("")      // Warning
    let sr2 = new System.IO.StringReader("")  // OK