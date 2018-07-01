namespace TicTacToe

type Player = X | O

module Player =
    let other x = 
        match x with
        | X -> O
        | O -> X




