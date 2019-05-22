namespace Practice.Recursive

module RecursiveTutorial =

    type Book ={title: string; price: decimal}
  
    type ChocolateType = Dark | Milk | SeventyPercent
    type Chocolate = {chocType: ChocolateType; price: decimal}
    type WrappingPaperStyle =
        | HappyBirthday
        | HappyHolidays
        | SolidColor
        
    type Gift =
        | Book of Book
        | Chocolate of Chocolate
        | Wrapped of Gift * WrappingPaperStyle
        | Boxed of Gift
        | WithACard of Gift * message:string
        
    let wolfHall = {title="Wolf Hall"; price=20m}
    let yummyChoc = {chocType=SeventyPercent; price=5m}
    let birthdayPresent = WithACard (Wrapped (Book wolfHall, HappyBirthday), "Happy Birthday")
    let christmasPresent = Wrapped (Boxed (Chocolate yummyChoc), HappyHolidays)
    
    let rec description gift =
        match gift with
        | Book book ->
            sprintf "'%s'" book.title
        | Chocolate choc ->
            sprintf "%A chocolate" choc.chocType
        | Wrapped (gift, style) ->
            sprintf "%s wrapped in %A paper" (description gift) style
        | Boxed gift ->
            sprintf "%s in a box" (description gift)
        | WithACard (gift, message) ->
            sprintf "%s with a card saying '%s'" (description gift) message
            
    birthdayPresent |> description |> ignore
    
    let rec totalCost gift =
        match gift with
        | Book book -> book.price
        | Chocolate choc -> choc.price
        | Wrapped (gift, style) -> (totalCost gift) + 0.5m
        | Boxed gift -> (totalCost gift) + 1.0m
        | WithACard (gift, message) -> (totalCost gift) + 2.0m
        
    birthdayPresent |> totalCost |> ignore
    christmasPresent |> totalCost |> ignore


    let rec cataGift fBook fChocolate fWrapped fBox fCard gift =
        let recurse = cataGift fBook fChocolate fWrapped fBox fCard
        
        match gift with
        | Book book ->
            fBook book
        | Chocolate choc ->
            fChocolate choc
        | Wrapped (gift, style) ->
            fWrapped (recurse gift, style)
        | Boxed gift ->
            fBox (recurse gift) 
        | WithACard (gift, message) ->
            fCard (recurse gift, message)
    
    let totalCostUsingCata gift =
        let fBook (book:Book) = book.price
        let fChocolate (choc:Chocolate) = choc.price
        let fWrapped (innerCost, style) = innerCost + 0.5m
        let fBox innerCost = innerCost + 1.0m
        let fWithACard (innerCost, message) = innerCost + 2.0m
        
        cataGift fBook fChocolate fWrapped fBox fWithACard gift
        
    let totalCost' = birthdayPresent |> totalCostUsingCata
    
    let descriptionUsingCata gift =
        let fBook (book:Book) = sprintf "'%s'" book.title
        let fChocolate (choc:Chocolate) = sprintf "%A chocolate" choc.chocType
        let fWrapped (innerText, style) = sprintf "%s wrapped in %A paper" innerText style
        let fBox innerText = sprintf "%s in a box" innerText
        let fCard (innerText, message) = sprintf "%s with a card saying '%s'" innerText message
        
        cataGift fBook fChocolate fWrapped fBox fCard gift
        
    
            
               