namespace fsharp.Practice.Railway.SqlDatabase
open System.Collections.Generic

[<AllowNullLiteral>]
type DbCustomer() =
    member val Id = 0 with get, set
    member val FirstName : string = null with get, set
    member val LastName : string = null with get, set
    member val Email : string = null with get, set
    

exception SqlException of string

type internal DbContext() =
    static let _data = new Dictionary<int, DbCustomer>()
    
    member this.Customers() : DbCustomer seq =
        upcast _data.Values
        
    member this.Update (customer:DbCustomer) =
        if _data.ContainsKey(customer.Id) |> not then
            raise (SqlException "KeyNotFound")
        else
            match customer.Id with
            | 42 ->
                raise (SqlException "Timeout")
                
            | _ ->
                _data.[customer.Id] <- customer
                
    member this.Insert (customer:DbCustomer) =
        if _data.ContainsKey(customer.Id) then
            raise (SqlException "DuplicateKey")
            
        else
            match customer.Id with
            | 42 ->
                raise (SqlException "Timeout")
            | _ ->
                _data.[customer.Id] <-customer