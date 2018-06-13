module RecursiveTutorial =
    type Book ={title:string; price:decimal}

    type ChocolateType = Dark | Milk | SeventyPercent
    type Chocolate ={ chocType: ChocolateType; price:decimal}

    type WrappingPaperStyle =
        | HappyBirthday
        | HappyHolidays
        | SolidColor

    type Gift =
        | Book of Book
        | Chocolate of Chocolate
        | Wrapped of Gift * WrappingPaperStyle
        | Boxed of Gift
        | WithCard of Gift * message:string

    // a Book
    let wolfHall ={title="Wolf Hall"; price=20m }

    // a Chocolate
    let yummyChoc = {chocType=SeventyPercent; price=5m}

    // A Gift
    let birthdayPresent = WithCard ((Wrapped (Book wolfHall, HappyBirthday)), "Happy Birthday")

    // A Gift
    let christmasPresent = Wrapped (Boxed (Chocolate yummyChoc), HappyHolidays)

    let rec description gift =
        match gift with
        | Book book ->
            sprintf "'%s'" book.title
        | Chocolate choc ->
            sprintf "%A chocolate" choc.chocType
        | Wrapped (innerGift, style) ->
            sprintf "%s wrapped in %A paper" (description innerGift) style
        | Boxed innerGift ->
            sprintf "%s in a box" (description innerGift)
        | WithCard (innerGift, message) ->
            sprintf "%s with a card saying '%s'" (description innerGift) message

    birthdayPresent |> description
    christmasPresent |> description

    let rec totalCost gift =
        match gift with 
        | Book book ->
            book.price
        | Chocolate choc ->
           choc.price
        | Wrapped (innerGift, style) ->
            (totalCost innerGift) + 0.5m
        | WithCard (innerGift, message) ->
            (totalCost innerGift) + 2.0m
        | Boxed innerGift ->
            (totalCost innerGift) + 1.0m
       
    birthdayPresent |> totalCost
    christmasPresent |> totalCost

    let rec whatsInside gift =
        match gift with
        | Book book ->
            "A book"
        | Chocolate choc ->
            "Some chocolate"
        | Wrapped (innerGift, style) ->
            whatsInside innerGift
        | Boxed innerGift ->
            whatsInside innerGift
        | WithCard (innerGift, _) ->
            whatsInside innerGift

    birthdayPresent |> whatsInside
    christmasPresent |> whatsInside

    let rec cataGift fBook fChocolate fWrapped fBox fCard gift =
        match gift with 
        | Book book -> 
            fBook book
        | Chocolate choc -> 
            fChocolate choc
        | Wrapped (innerGift,style) -> 
            let innerGiftResult = cataGift fBook fChocolate fWrapped fBox fCard innerGift
            fWrapped (innerGiftResult,style)
        | Boxed innerGift -> 
            let innerGiftResult = cataGift fBook fChocolate fWrapped fBox fCard innerGift
            fBox innerGiftResult 
        | WithCard (innerGift,message) -> 
            let innerGiftResult = cataGift fBook fChocolate fWrapped fBox fCard innerGift
            fCard (innerGiftResult,message) 

    let totalCostUsingCata gift =
        let fBook (book:Book) =
            book.price
        let fChocolate (choc:Chocolate) =
            choc.price
        let fWrapped (innerCost, style) =
            innerCost + 0.5m
        let fBox innerCost =
            innerCost + 1.0m
        let fCard (innerCost, message) =
            innerCost + 2.0m
        
        cataGift fBook fChocolate fWrapped fBox fCard gift

    birthdayPresent |> totalCostUsingCata
    
    let descriptionUsingCata gift =
        let fBook (book:Book) =
            sprintf "'%s'" book.title
        let fChocolate (choc:Chocolate) =
            sprintf "%A chocolate" choc.chocType
        let fWrapped (innerText, style) =
            sprintf "%s wrapped in %A paper" innerText style
        let fBox innerText =
            sprintf "%s in a box" innerText
        let fCard (innerText, message) =
            sprintf "%s with a card saying '%s'" innerText message

        cataGift fBook fChocolate fWrapped fBox fCard gift


    let rec ``cataGift reractor1`` fBook fChocolate fWrapped fBox fCard gift =
        let recurse = ``cataGift reractor1`` fBook fChocolate fWrapped fBox fCard
        match gift with
        | Book book ->
            fBook book
        | Chocolate choc ->
            fChocolate choc
        | Wrapped (innerGift, style) ->
            let innerGiftResult = recurse innerGift
            fWrapped innerGiftResult
        | Boxed innerGift ->
            let innerGiftResult = recurse innerGift
            fBox innerGiftResult
        | WithCard (innerGift, message) ->
            let innerGiftResult = recurse innerGift
            fCard innerGiftResult

    let rec ``cataGift reractor2`` fBook fChocolate fWrapped fBox fCard gift :'r =
        let recurse = ``cataGift reractor2`` fBook fChocolate fWrapped fBox fCard
        match gift with
        | Book book ->
            fBook book
        | Chocolate choc ->
            fChocolate choc
        | Wrapped (innerGift, style) ->
            fWrapped (recurse innerGift, style)
        | Boxed innerGift ->
            fBox (recurse innerGift)
        | WithCard (innerGift, message) ->
            fCard (recurse innerGift, message)


    type Gift2 =
        | Book of Book
        | Chocolate of Chocolate
        | Wrapped of Gift2 * WrappingPaperStyle
        | Boxed of Gift2
        | WithACard of Gift2 * message:string

    //let rec ``cataGift 1`` (fBook: Book -> 'r) (fChocolate: Chocolate -> 'r) (fWrapped: (Gift * WrappingPaperStyle) -> 'r) (fBox: Gift2 -> 'r) (gift: Gift2) :'r =
    let rec ``cataGift 1`` fBook fChocolate fWrapped fBox gift :'r =
        let recurse = ``cataGift 1`` fBook fChocolate fWrapped fBox 
        match gift with 
        | Book book -> 
            fBook book
        | Chocolate choc -> 
            fChocolate choc
        | Wrapped (gift,style) -> 
            fWrapped (recurse gift,style)
        | Boxed gift -> 
            fBox (recurse gift)
        | WithACard (gift, message) ->
            recurse gift

    let rec (|Book|Chocolate|Wrapped|Boxed|) gift =
        match gift with
        | Gift2.Book book ->
            Book book
        | Gift2.Chocolate choc ->
            Chocolate choc
        | Gift2.Wrapped (gift, style) ->
            Wrapped (gift, style)
        | Gift2.Boxed gift ->
            Boxed gift
        | Gift2.WithACard (gift, message) ->
            (|Book|Chocolate|Wrapped|Boxed|) gift

    let rec whatsInside1 gift =
        match gift with
        | Gift2.Book book ->
            "A book"
        | Gift2.Chocolate choc ->
            "Some chacolate"
        | Gift2.Wrapped (gift, style) ->
            whatsInside1 gift
        | Gift2.Boxed gift ->
            whatsInside1 gift

    let handleContents fBook fChocolate gift =
        let fWrapped (innerGiftResult,style) =   
            innerGiftResult
        let fBox innerGiftResult = 
            innerGiftResult
        let fCard (innerGiftResult,message) = 
            innerGiftResult
        // call the catamorphism
        cataGift fBook fChocolate fWrapped fBox fCard gift

    birthdayPresent |> handleContents 
    (fun book -> "The book you wanted for your birthday") 
    (fun choc -> "Your fave chocolate")
// Result => "The book you wanted for your birthday"

    type GiftMinusChocolate =
        | Book of Book
        | Apology of string
        | Wrapped of GiftMinusChocolate * WrappingPaperStyle


    let removeChocolate gift =
        let fBook (book:Book) = 
            Book book
        let fChocolate (choc:Chocolate) = 
            Apology "sorry I ate your chocolate"
        let fWrapped (innerGiftResult,style) = 
            Wrapped (innerGiftResult,style) 
        let fBox innerGiftResult = 
            innerGiftResult
        let fCard (innerGiftResult,message) = 
            innerGiftResult
        // call the catamorphism
        cataGift fBook fChocolate fWrapped fBox fCard gift

    
    let deepCopy gift : Gift =
        let fBook book = 
            Gift.Book book 
        let fChocolate (choc:Chocolate) = 
            Gift.Chocolate choc
        let fWrapped (innerGiftResult,style) = 
            Gift.Wrapped (innerGiftResult,style) 
        let fBox innerGiftResult = 
            Gift.Boxed innerGiftResult
        let fCard (innerGiftResult,message) = 
            Gift.WithCard (innerGiftResult,message) 
        // call the catamorphism
        cataGift fBook fChocolate fWrapped fBox fCard gift


    christmasPresent |> deepCopy

    let upgradeChocolate gift =
        let fBook = Gift.Book 
        let fChocolate (choc:Chocolate) = 
            Gift.Chocolate {choc with chocType = SeventyPercent}
        let fWrapped = Gift.Wrapped 
        let fBox = Gift.Boxed 
        let fCard = Gift.WithCard 
        // call the catamorphism
        cataGift fBook fChocolate fWrapped fBox fCard gift

    let cheapChoc = Gift.Boxed (Gift.Chocolate {chocType=Milk; price=5m})
    cheapChoc |> upgradeChocolate

