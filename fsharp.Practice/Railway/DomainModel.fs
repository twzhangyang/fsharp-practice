module fsharp.Practice.Railway.DomainModel
open fsharp.Practice.Railway.Rop
open fsharp.Practice.Railway.DomainPrimitiveTypes

 type PersonalName = {
     FirstName: String10.T
     LastName: String10.T
 }
 
 type Customer = {
     Id: CustomerId.T
     Name: PersonalName
     Email: EmailAddress.T
 }
 
 type DomainMessage =
     | CustomerIsRequired
     | CustomerIdMustBePositive
     | FirstNameIsRequired
     | FirstNameMustNotBeMoreThan10Chars
     | LastNameIsRequired
     | LastNameMustNotbeMoreThan10Chars
     | EmailIsRequired
     | EmailMustNotBeMoreThan20Chars
     | EmailMustContainAtSign
     
     | EmailAddressChanged of string * string
     | CustomerNotFound
     | SqlCustomerIsInvalid
     | DatabaseTimeout
     | DatabaseError of string
     
let createFirstName firstName =
    let map = function
        | StringError.Missing -> FirstNameIsRequired
        | MustNotBeLongerThan _ -> FirstNameMustNotBeMoreThan10Chars
        | DoesntMatchPattern _ -> failwithf "not expecting doesntMatchPattern for firstName"
    
    String10.create firstName |> mapMessageR map
    
let createLastName lastName =
    let map = function
    | StringError.Missing -> EmailIsRequired
    | MustNotBeLongerThan _ -> EmailMustNotBeMoreThan20Chars
    | DoesntMatchPattern _ -> EmailMustContainAtSign
    
    String10.create lastName |> mapMessageR map
    
let createEmail email =
    let map = function
    | StringError.Missing -> EmailIsRequired
    | MustNotBeLongerThan _ -> EmailMustNotBeMoreThan20Chars
    | DoesntMatchPattern _ -> EmailMustContainAtSign
    
    EmailAddress.create email |> mapMessageR map
    
let createCustomerId customerId =
    let map = function
    | IntegerError.Missing -> CustomerIsRequired
    | MustBePositiveInteger _ -> CustomerIdMustBePositive
    
    CustomerId.create customerId |> mapMessageR map
    
let createPersonalName firstName lastName =
    {FirstName= firstName; LastName = lastName}
    
let createCustomer customerId name email =
    {Id = customerId; Name=name; Email=email}
    
    
    
     