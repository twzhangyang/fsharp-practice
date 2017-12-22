
module Seq=
    let seq1=seq{0..2..10}
    let seq2=seq{for i in [1..10]->i*i}

    let board=
        seq{
            for row in [1..8] do
                for column in [1..8] do
                    yield pown (-1)(row+column)
        }
        |>Seq.map(fun i->if i= -1 then "x " else "o")

    let print i v=
        if i>0 && i%8=0 then
            printfn ""
        printf "%s" v

    Seq.iteri print board
