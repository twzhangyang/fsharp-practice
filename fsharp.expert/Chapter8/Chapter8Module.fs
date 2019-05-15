module fsharp.expert.Chapter8.Chapter8Module 
open System.Security.Authentication
open FSharp.Data
open FSharp.Data

[<Literal>]
let customersXmlSample = """
      <Customers>
        <Customer name="ACME">
            <Order Number="A012345">
                <OrderLine Item="widget" Quantity="1" />
            </Order>
            <Order Number="A012345">
                <OrderLine Item="widget" Quantity="1" />
            </Order>
        </Customer>
        
        <Customer name="AAA">
            <Order Number="1234">
                <OrderLine Item="aaaa" Quantity="2" />
                <OrderLine Item="aaaa" Quantity="2" />
            </Order>
        </Customer>
      </Customers>    
      """

type InputXml = XmlProvider<customersXmlSample>


let orders() =
    let inputs = InputXml.GetSample().Customers
    [for customer in inputs do 
        for order in customer.Orders do 
            for line in order.OrderLines do 
                yield (customer.Name, order.Number, line.Item, line.Quantity)
    ]
    
[<Literal>]    
let customerjsonSample = """
    {   "customers" :
        [
            {  "name" : "Apple Store",
                "orders":
                    [
                        {  "number" : "123123", "item" : "iphone5", "quantity" : 18732},
                        {  "number" : "123123", "item" : "iphone5", "quantity" : 18732}
                    ]
            },   
            {  "name" : "Apple Store",
                "orders":
                    [
                        {  "number" : "123123", "item" : "iphone5", "quantity" : 18732},
                        {  "number" : "123123", "item" : "iphone5", "quantity" : 18732}
                    ]
            }  
        ]
        
    }
"""

type CustomersJson = JsonProvider<customerjsonSample>
let customerNames = [ for c in CustomersJson.GetSample().Customers -> c.Name]

let newOrder = CustomersJson.Order(number = 12312, item = "nexus7", quantity = 1231)
let newCustomer = CustomersJson.Customer(name = "FabPhone", orders = [| newOrder |])
let jsonText = newCustomer.JsonValue.ToString()


type Term =
    | Term of int * string * int
    | Const of int
    
type Polynomial = Term list

let a = [Term ( 1, "x", 5); Term(-2, "x", 3); Const 20]

type Token =
    | ID of string
    | INT of int
    | HAT
    | PLUS
    | MINUS
    
let regex s = new System.Text.RegularExpressions.Regex(s)
let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)"
let tokenize (s : string) =
    [for x in tokenR.Match(s).Groups.["token"].Captures do
        let token =
            match x.Value with 
            | "^" -> HAT
            | "-" -> MINUS
            | "+" -> PLUS
            | s when System.Char.IsDigit s.[0] -> INT (int s)
            | s -> ID s
        yield  token
    ]
    
let a'  = tokenize "x^5 - 2x^3 + 20"
type polynomial = Term list
type TokenStream = Token list

let tryToken ( src: TokenStream) =
    match src with 
    | tok :: rest -> Some(tok, rest)
    | _ -> None
    
let parseIndex src =
    match tryToken src with 
        | Some (HAT, src) -> 
            match tryToken src with 
            | Some (INT num2, src ) -> 
                num2, src
            | _ -> failwith "expected an integer after '^'"

        | _ -> 1, src


