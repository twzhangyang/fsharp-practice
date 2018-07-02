namespace PropertyTests.ChoseProperties

open FsCheck.Xunit
open FsCheck
open System

module DifferentPathsSameDestination =

    [<Property>]
    let ``+1 then sort should be same as sort then +1`` aList =
        let add1 x = x + 1

        let result1 = aList |> List.sort |> List.map add1
        let result2 = aList |> List.map add1 |> List.sort

        result1 = result2

    [<Property>]
    let ``append minValue then sort should be same as sort then prepend minValue`` aList =
        let minValue = Int32.MinValue

        let appendThenSort = (aList @ [minValue]) |> List.sort
        let sortThenAppend = minValue :: (aList |> List.sort)

        appendThenSort = sortThenAppend

    [<Property>]
    let ``negate then sort should be same as sort then negate then reverse`` aList =
        let negate x = x * -1
        
        let negateThenSort = aList |> List.map negate |> List.sort
        let sortThenNegateThenReverse = aList |> List.sort |> List.map negate |> List.rev

        negateThenSort = sortThenNegateThenReverse

    [<Property>]
    let ``append any value then reverse should be same as reverse then prepend same value`` anyValue aList =
        let appendThenReverse = (aList @ [anyValue]) |> List.rev
        let reverseThenPrepend = anyValue :: (aList |> List.rev )

        appendThenReverse = reverseThenPrepend

module ThereAndBackAgian =
    let ``reverse then reverse should be same as original`` aList =
        let reverseThenReverse = aList |> List.rev |> List.rev
        reverseThenReverse =  aList


module HardToProveEasyToVerify =
    
    [<Property>]
    let ``concatting the elements of a string split by commas recreates the original string`` aListOfStrings =
        let addWithComma s t = s + "." + t
        let originalString = aListOfStrings |> List.fold addWithComma ""

        let tokens = originalString.Split [| ',' |]
        let recombinedString = tokens |> Array.fold addWithComma ""

        originalString = recombinedString

    [<Property>]
    let ``adjacent paris from a list should be ordered`` (aList:int list) =
        let paris = aList |> List.sort |> Seq.pairwise
        paris |> Seq.forall (fun (x,y) -> x <=y)

    [<Property>]
    let ``list is osrted`` (aList: int list) =
        let prop1 = ``adjacent paris from a list should be ordered``
        let prop2 = ``concatting the elements of a string split by commas recreates the original string``

        prop1 .&. prop2

    [<Property>]
    let ``list is osrted2`` (aList: int list) =
        let prop1 = ``adjacent paris from a list should be ordered`` |@ "adjacent paris from a list should be ordered"
        let prop2 = ``concatting the elements of a string split by commas recreates the original string`` |@ "a sorted list has some contents as the original list"

        prop1 .&. prop2

module SomethingsNeverChange =
    open System.Linq
    open System.Linq
    
    [<Property>]
    let ``sort should have some length as original`` (alist:int list) =
        let sorted = alist |> List.sort
        List.length sorted = List.length alist

    let rec insertElement anElement aList =
        seq {
            match aList with
            | [] -> yield [anElement]
            | first::rest ->
                yield anElement::aList
                for subList in insertElement anElement rest do
                    yield first::subList
            }

    let rec permutations aList =
        seq {
            match aList with
            | [] -> yield []
            | first::rest ->
                for subList in permutations rest do 
                    yield! insertElement first subList
        }

    //[<Property>]
    //let ``a sorted list is always a permutation of the original list`` (aList:int list) =
    //    let sorted = aList |> List.sort
    //    let permutationOfOriginalList = permutations aList

    //    permutationOfOriginalList |> Seq.exists (fun permutation -> permutation = sorted)

    let rec ``first element is <= than second, and tail is also sorted`` (aList:int list) =
        let sortedList = aList |> List.sort
        match sortedList with
        | [] -> true
        | [first] -> true
        | [first;second] ->
            first <= second
        | first::second::tail ->
            first <= second &&
            let subList = second::tail
            ``first element is <= than second, and tail is also sorted`` subList

    [<Property>]
    let ``sorting twice gives the same result as sorting once`` (aList:int list) =
        let sortedOnce = aList |> List.sort
        let sortedTwice = aList |> List.sort |> List.sort

        sortedOnce = sortedTwice

