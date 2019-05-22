namespace Pactice.Recursive

module listRec =
    let foldFactorial x = [ 1 .. x ] |> List.fold (fun state x -> state + x) 0
    let reduceFactorial x = [ 1 .. x ] |> List.reduce (fun x y -> x + y)
    
    
    
module Tree =    
    type Tree<'a> =
        | Node of Tree<'a> * 'a
        | Leaf
    
    let rec fold (f: 'state -> Tree<'a> -> 'state) (acc:'state) (t:Tree<'a>) :'state =
        let acc' = f acc t 
        
        match t with
        | Node(t1, _) -> fold f acc' t1
        | Leaf -> acc'
        
    
    let sum t =
        let folder state t' : int =
            match t' with
            | Node (_, v) -> state + v
            | Leaf -> state
            
        fold folder 0 t
        
    let tree = Node(Node(Leaf,1),2)
    
    tree |> sum |> ignore
    
