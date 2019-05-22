module fsharp.Practice.Railway.DataAccessLayer
open System
open fsharp.Practice.Railway.DomainModel
open fsharp.Practice.Railway.DomainPrimitiveTypes
open fsharp.Practice.Railway.Rop
open fsharp.Practice.Railway.SqlDatabase

type ICustomerDao =
    abstract GetAll : unit -> RopResult<Customer seq, DomainMessage>
    
    abstract GetById : CustomerId.T -> RopResult<Customer, DomainMessage>
    
    abstract Upsert : Customer -> RopResult<unit, DomainMessage>
    
let fromDbCustomer (dbCustomer: DbCustomer) =
    if dbCustomer = null then
        fail SqlCustomerIsInvalid
    else
        let idOrError = createCustomerId dbCustomer.Id
        
        let firstNameOrError = createFirstName dbCustomer.FirstName
        let lastNameOrError = createLastName dbCustomer.LastName
//        let personalNameOrError = lift2R createPersonalName firstNameOrError lastNameOrError
        let personalNameOrError' = createPersonalName
                                   <!> createFirstName dbCustomer.FirstName
                                   <*> createLastName dbCustomer.LastName
                                   
        
        
//        let emailOrError = createEmail dbCustomer.Email
//        let customerOrError = lift3R createCustomer idOrError personalNameOrError emailOrError
//        customerOrError

        createCustomer
        <!> createCustomerId dbCustomer.Id
        <*> personalNameOrError'
        <*> createEmail dbCustomer.Email
        

let fromDbCustomerIdiomatic (dbCustomer: DbCustomer) =
    if dbCustomer = null then
        fail SqlCustomerIsInvalid
        
    else
        let nameOrError =
            createPersonalName
            <!> createFirstName dbCustomer.FirstName
            <*> createLastName dbCustomer.LastName
            
        createCustomer
        <!> createCustomerId dbCustomer.Id
        <*> nameOrError
        <*> createEmail dbCustomer.Email
        
let toDbCustomer(cust:Customer) =
    let custIdInt = cust.Id |> CustomerId.apply id
    let dbCustomer = DbCustomer()
    dbCustomer.Id <- custIdInt
    dbCustomer.FirstName <- cust.Name.FirstName |> String10.apply id
    dbCustomer.LastName <- cust.Name.LastName |> String10.apply id
    dbCustomer.Email <- cust.Email |> EmailAddress.apply id
    dbCustomer
    
let (|KeyNotFound|DuplicateKey|Timeout|Other|) (ex:SqlException) =
    match ex.Data0 with
    | "KeyNotFound" -> KeyNotFound
    | "DuplicateKey" -> DuplicateKey
    | "Timeout" -> Timeout
    | _ -> Other
    
let failureFromException (ex:SqlException) =
    match ex with
    | Timeout ->
        fail DatabaseTimeout
    | _ ->
        fail (DatabaseError ex.Message)
        
    
type CutomerDao() =
    interface ICustomerDao with
        member this.GetAll() =
            let db  = new DbContext()
            let fSuccess (x,_) = Some x
            let fFailure _ = None
            
            try
                db.Customers()
                |> Seq.map fromDbCustomer
//                |> Seq.filter  (either (fun (x, _) -> true) (fun _ -> false)) 
                |> Seq.choose (either fSuccess fFailure)
                |> succeed
            with
            | :? SqlException as ex -> failureFromException ex
            
        member this.GetById customerId =
            let db = new DbContext()
            
            let custIdInt = customerId |> CustomerId.apply id
            
            try
                db.Customers()
                |> Seq.tryFind (fun sql -> sql.Id = custIdInt)
                |> Option.map fromDbCustomer
                |> failIfNoneR CustomerNotFound
                
            with
            | :? SqlException as ex -> failureFromException ex
            
        
        member this.Upsert (customer:Customer) =
            let db = new DbContext()
            
            try
                let newDbCust = toDbCustomer customer
                let custIdInt = customer.Id |> CustomerId.apply id
                let existingDbCustOpt =
                    db.Customers()
                    |> Seq.tryFind (fun sql -> sql.Id = custIdInt)
                
                match existingDbCustOpt with
                | None ->
                    db.Insert(newDbCust)
                    succeed ()
                    
                | Some existingDbCust ->
                    db.Update(newDbCust)
                    if newDbCust.Email <> existingDbCust.Email then
                        let event = EmailAddressChanged (existingDbCust.Email, newDbCust.Email)
                        succeedWithMsg () event
                        
                    else
                        succeed ()
            with
            | :? SqlException as ex -> failureFromException ex