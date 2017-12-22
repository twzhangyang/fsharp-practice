namespace InteropCSharp

open CSharpLibrary

type Consumer()=
    let c1=new Numbers()
    member this.X=c1.FirstCountingNumber()

    interface ICanAddNumbers with
        member this.Add(a,b)=a+b


