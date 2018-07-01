module FSharp.PropertyTests.DimondTests

open System
open FsCheck.Xunit
open FsCheck
open System

type Letters =
   static member Char() =
       Arb.Default.Char()
       |> Arb.filter (fun c -> 'A' <= c && c <= 'Z')


type DimondPropertyAttribute () =
    inherit PropertyAttribute(Arbitrary=[| typeof<Letters> |])

//[<Property(Arbitrary=[| typeof<Letters> |])>]

[<DimondProperty>]
let ``Dimond is non-empty`` (letter:char) =
    printfn "%c" letter
    Console.WriteLine letter
    let actual = Dimond.make letter

    not (String.IsNullOrWhiteSpace actual)

let split (x:string) =
    x.Split (System.Environment.NewLine, StringSplitOptions.None)

[<DimondProperty>]
let ``First row contains 'A'`` (letter:char) =
    let actual =Dimond.make letter

    let rows = split actual
    rows |> Seq.head |> Seq.exists ((=) 'A')

let leadingSpaces (x : string) =
    let indexOfNonSpace = x.IndexOfAny [| 'A' .. 'Z'|]
    x.Substring(indexOfNonSpace+1)

let trailingSpaces (x: string) =
    let lastIndexOfNonSpace = x.LastIndexOfAny [| 'A' .. 'Z'|]
    x.Substring(0,lastIndexOfNonSpace)

[<DimondProperty>]
let ``All rows must have a symetric contour`` (letter:char) =
    let actual = Dimond.make letter

    let rows = split actual
    rows |> Array.forall (fun r -> (leadingSpaces r) = (trailingSpaces r))

let trim (x : string ) =
    x.Trim()

[<DimondProperty>]
let ``Top of figure has correct letters in correct order`` (letter : char) =
    let actual = Dimond.make letter

    let expected = ['A' .. letter]
    let rows = split actual
    let firstNonWhiteSpaceLetters =
        rows
        |> Seq.take expected.Length
        |> Seq.map trim
        |> Seq.map Seq.head
        |> Seq.toList
    expected = firstNonWhiteSpaceLetters


[<DimondProperty>]
let ``Figure is symmetric around the horizontal axis`` (letter:char) =
    let actual = Dimond.make letter
    let rows = split actual
    let topRows = 
        rows
        |> Seq.takeWhile (fun x -> not (x.Contains(string letter)))
        |> Seq.toList

    let bottomRows =
        rows
        |> Seq.skipWhile ( fun x -> not (x.Contains(string letter)))
        |> Seq.skip 1
        |> Seq.rev
        |> Seq.toList
    
    topRows = bottomRows

[<DimondProperty>]
let ``Diamond is as wide as it's high`` (letter:char) =
    let actual = Dimond.make letter

    let rows = split actual
    let expected = rows.Length
    rows |> Array.forall (fun x -> x.Length = expected)

[<DimondProperty>]
let ``All rows except top and bottom have two identical letters`` (letter:char) =
    let actual = Dimond.make letter

    let isTwoIdenticalLetters x =
        let hasIdenticalLetters = x |> Seq.distinct |> Seq.length = 1
        let hasTwoLetters = x |> Seq.length = 2
        hasIdenticalLetters && hasTwoLetters
    
    let rows = split actual
    rows
    |> Seq.filter (fun c -> not (c.Contains("A")))
    |> Seq.map trim
    |> Seq.forall isTwoIdenticalLetters