namespace fsharp.Practice.Railway.Controllers
open System
open System.Net
open System.Web.Http
open System.Web.Http.Results
open fsharp.Practice.Railway
open fsharp.Practice.Railway.DomainModel
open fsharp.Practice.Railway.Rop
open fsharp.Practice.Railway.DtoConverter

module CustomersControllerHelper =

    type ResponseMessage =
        | NotFound
        | BadRequest of string
        | InternalServerError of string
        | DomainEvent of DomainMessage
        
    let classify msg =
        match msg with
        | CustomerIsRequired
        | CustomerIdMustBePositive
        | FirstNameIsRequired
        | FirstNameIsRequired
        | FirstNameMustNotBeMoreThan10Chars
        | LastNameIsRequired
        | LastNameMustNotbeMoreThan10Chars
        | EmailIsRequired
        | EmailMustContainAtSign
        | EmailMustNotBeMoreThan20Chars ->
            BadRequest (sprintf "%A" msg)
            
        | EmailAddressChanged _ ->
            DomainEvent msg
        
        | CustomerNotFound ->
            NotFound
        
        | SqlCustomerIsInvalid
        | DatabaseTimeout
        | DatabaseError _->
            InternalServerError (sprintf "%A" msg)
            
        
    let primaryResponse msgs =
        msgs
        |> List.map classify
        |> List.sort
        |> List.head
        
    
    let badRequestToStr msgs =
        msgs
        |> List.map classify
        |> List.choose (function BadRequest s -> Some s | _ -> None)
        |> List.map (sprintf "ValidationError: %s;")
        |> List.reduce (+)
        
    let domainEventsToStr msgs =
        msgs
        |> List.map classify
        |> List.choose (function DomainEvent s -> Some s | _ -> None)
        |> List.map (sprintf "DomainEvent: %A")
        |> List.reduce (+)
        
    let toHttpResult (controller:ApiController) msgs : IHttpActionResult =
        match primaryResponse msgs with
        | NotFound ->
            upcast NotFoundResult(controller)
        | BadRequest _ ->
            let validationMsg = badRequestToStr msgs
            upcast NegotiatedContentResult(HttpStatusCode.BadRequest, validationMsg, controller)
        
        | InternalServerError msg ->
            upcast NegotiatedContentResult(HttpStatusCode.InternalServerError, msg, controller)
        | DomainEvent _ ->
            let eventsMsg = domainEventsToStr msgs
            upcast NegotiatedContentResult(HttpStatusCode.OK, eventsMsg, controller)
            
type CustomersController(fsDao:DataAccessLayer.ICustomerDao) as this =
    inherit ApiController()
    
    let getByIdR = bindR fsDao.GetById
    let customerToDtoR = mapR DtoConvert.customerToDto
    let dtoToCustomerR = bindR DtoConvert.dtoToCustomer
    let upsertCustomerR = bindR fsDao.Upsert
    let createCustomerIdR = bindR createCustomerId
    
    let ok content =
        if content = box () then
            OkResult(this) :> IHttpActionResult
        else
            NegotiatedContentResult(HttpStatusCode.OK, content, this) :> IHttpActionResult
        
    let okR result = mapR ok result   
        
    let toHttpResult result =
        result |> valueOrDefault (CustomersControllerHelper.toHttpResult this)
        
        
    let log format (objs:obj[]) =
        Console.WriteLine("log" + format, objs)
        
    let logSuccessR format result =
        let logSuccess objj = log format [|obj|]
        
        result |> successTee logSuccess
        
    let logFailureR result =
        let logError err = log "Error: {0}" [| sprintf "%A" err |]
        
        result |> failureTee (Seq.iter logError)
        
    let notifyCustomerWhenEmailChangedR =
        let detectEvent = function
            | EmailAddressChanged (oldEmail, newEmail) -> Some (oldEmail, newEmail)
            | _ -> None
            
        let nofifyCustomer (oldEmail, newEmail) =
            log "Email changed from {0} to {1}" [|oldEmail; newEmail|]
            
        successTee (fun (_, msgs) ->
            msgs
            |> List.choose detectEvent
            |> List.iter nofifyCustomer
            )
    
    [<Route("customerE/{customerId}")>]
    [<HttpGet>]
    member this.GetWithErrorHandling(customerId:int) : IHttpActionResult =
        succeed customerId
        |> logSuccessR "GetWithErrorHandling {0}"
        |> createCustomerIdR
        |> getByIdR
        |> customerToDtoR
        |> logFailureR
        |> okR
        |> toHttpResult
        
        
    [<Route("customerE/{customerId")>]
    [<HttpPost>]
    member this.Post(customerId:int, [<FromBody>]dto:CustomerDto) : IHttpActionResult =
        dto.Id <- customerId
        
        succeed dto
        |> logSuccessR "POST with {0}"
        |> dtoToCustomerR
        |> upsertCustomerR
        |> logFailureR
        |> notifyCustomerWhenEmailChangedR
        |> okR
        |> toHttpResult
       
       
    
