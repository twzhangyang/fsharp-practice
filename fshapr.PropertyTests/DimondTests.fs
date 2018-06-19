module FSharp.PropertyTests.DimondTests

open System
open FsCheck.Xunit
open Xunit

[<Property>]
let ``Dimond is non-empty`` (letter:string) =
    let actual = Dimond.make letter

    not (String.IsNullOrEmpty letter)

