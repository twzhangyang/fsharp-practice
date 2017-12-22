module shoppingCart =

    type CartItem = string
    type EmptyState = NoItems
    type ActiveState = { UnpaidItems: CartItem List }
    type PaidForState = { PaidItems: CartItem List; Payment: decimal }
    type Cart =
        | Empty of EmptyState
        | Active of ActiveState
        | PaidFor of PaidForState
    
    let addToEmptyState item =
        Cart.Active { UnpaidItems=[item] }

    let addToActiveState state itemToAdd =
        let newList = itemToAdd :: state.UnpaidItems
        Cart.Active { state with UnpaidItems=newList }

    let removeFromActiveState state itemToRemove =
        let newList = 
            state.UnpaidItems 
            |> List.filter (fun i-> i<>itemToRemove)

        match newList with
            | [] -> Cart.Empty NoItems
            | _ -> Cart.Active { state with UnpaidItems=newList }
        
    let payForActiveState state amount =
        Cart.PaidFor { PaidItems=state.UnpaidItems; Payment=amount }

    type EmptyState with
        member this.Add = addToEmptyState
    
    type ActiveState with
        member this.Add = addToActiveState this
        member this.Remove = removeFromActiveState this
        member this.Pay = payForActiveState

    let addItemToCart cart item =
        match cart with
        | Empty state -> state.Add item
        | Active state -> state.Add item
        | PaidFor state -> printfn "Error, the cart is paid for"; cart
    
    let removeItemFromCart cart item =
        match cart with
        | Empty state -> printfn "Error: the cart is empty" 
                         cart
        | Active state -> state.Remove item
        | PaidFor state -> printfn "Error: the cart is paid for" 
                           cart

    let dispalyCart cart =
        match cart with
        | Empty state -> printfn "The cart is empty"
        | Active state -> printfn "the cart contains %A unpaid items" state.UnpaidItems
        | PaidFor state -> printfn "The cart contains %A paid items. Amount paid: %f" state.PaidItems state.Payment

    type Cart with
        static member NewCart = Cart.Empty NoItems
        member this.Add = addItemToCart this
        member this.Remove = removeItemFromCart this
        member this.Display = dispalyCart this
