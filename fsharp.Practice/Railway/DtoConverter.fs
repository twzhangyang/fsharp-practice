namespace fsharp.Practice.Railway.DtoConverter

[<AllowNullLiteral>]
type CustomerDto () =
    member val Id = 0 with get, set
    member val FirstName : string = null with get,set
    member val LastName : string = null with get, set
    member val Email : string = null with get, set
    
module DtoConvert =
    open fsharp.Practice.Railway.Rop
    open fsharp.Practice.Railway.DomainModel
    open fsharp.Practice.Railway.DomainPrimitiveTypes

    let dtoToCustomer (dto: CustomerDto) =
        if dto = null then
            fail CustomerIsRequired
        else
            let idOrError = createCustomerId dto.Id
            let personalNameOrError = lift2R createPersonalName (createFirstName dto.FirstName) (createLastName dto.LastName)
            let customerOrError = lift3R createCustomer idOrError personalNameOrError (createEmail dto.Email)
            customerOrError
    
    let dtoToCustomerIdiomatic (dto:CustomerDto) =
        if dto = null then
            fail CustomerIsRequired
        else
            let nameOrError =
                createPersonalName
                <!> createFirstName dto.FirstName
                <*> createLastName dto.LastName
                
            createCustomer
            <!> createCustomerId dto.Id
            <*> nameOrError
            <*> createEmail dto.Email
            
    let customerToDto(cust:Customer) =
        let custIdInt = cust.Id |> CustomerId.apply id
        let customerDto = CustomerDto()
        customerDto.Id <- custIdInt
        customerDto.FirstName <- cust.Name.FirstName |> String10.apply id
        customerDto.LastName <- cust.Name.LastName |> String10.apply id
        customerDto.Email <- cust.Email |> EmailAddress.apply id
        
        customerDto
        
        
            
        
     