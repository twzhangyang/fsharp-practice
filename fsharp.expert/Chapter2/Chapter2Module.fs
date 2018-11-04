module fsharp.expert.Chapter2.Chapter2Module
open Xunit

let splitAtSpaces (text:string) =
    text.Split ' '
    |> Array.toList
        
let wordCount text =
    let words = splitAtSpaces text
    let numWords = words.Length
    let distinctWords = List.distinct words
    let numDups = numWords -  distinctWords.Length
    (numWords, numDups)
    
    
let showWordCount text =
    let numWords, numDups = wordCount text
    printfn "--> %d words in the text" numWords 
    printfn "--> %d duplicate words" numDups
    

[<Fact>]    
let ``use tuples`` ()=
    let site1 = ("www.cnn.com", 10)
    let site2 = ("www.bbc.com", 5)
    
    let a = fst site1
    let b = snd site1
    Assert.Equal(a, "www.cnn.com")
    Assert.Equal(b, 10)
    
    let a1, b1 = site2
    Assert.Equal(a1, "www.bbc.com")
    Assert.Equal(b1, 5)
    
    let c, d, e = (1, 2, 3)
    Assert.Equal(c, 1)
    
 
[<Fact>]
let ``first program`` () =
    let (numWords, numDups) = wordCount "All the king's horses and all the king's men "
    Assert.Equal(numWords, 10)
    Assert.Equal(numDups, 2)
 

        

    
        
        

