namespace PropertyTests

open System
open FsCheck.Xunit
open FsCheck

module TestsDemo =
    open Expecto

    let add x y =
        x + y


    let commutativePropery (x,y) =
        let result1 =  add x y
        let result2 =  add y x

        result1 = result2

    [<Property>]
    let ``evaluate commmutative`` x y =
        ((x,y) <> (0,0)) ==>

        let result1 = add x y
        let result2 = add y x

        result1 = result2
            

