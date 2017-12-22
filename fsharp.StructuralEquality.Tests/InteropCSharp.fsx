#r @"bin\Debug\CSharpLibrary.dll"

open CSharpLibrary
open System

{new ICanAddNumbers
    with member this.Add (a,b)=a+b}




