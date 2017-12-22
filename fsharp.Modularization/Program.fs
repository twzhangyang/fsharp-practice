// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace fsharp.Modularization
open fsharp.Modularization.NamespaceB


module Main=
    [<EntryPoint>]
    let main argv = 
        let num=Service.getNum 10
        printfn "get a value: %d" num

        printfn "%A" argv
        0 // return an integer exit code
