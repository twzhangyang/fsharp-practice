module FSharp.PropertyTests.DimondTests

open System
open FsCheck.Xunit
open Xunit

[<Property>]
let ``Dimond is non-empty`` (letter:char) =
    let actual = Dimond.make letter

    not (String.IsNullOrWhiteSpace actual)

