namespace TicTacToe.Tests
open FsCheck
open Expecto

module GameTests =
    open TicTacToe

    let tests =
        testList "Domain" [
            testProperty "Other player is not equal to player" <|
            fun x -> Expect.notEqual x (Player.other x) "other should be different from original"
        ]





