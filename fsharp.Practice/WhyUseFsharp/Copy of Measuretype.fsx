
module MeasureType =
    type EmailAddress = EmailAddress of string

    let sendEmail (EmailAddress email) = 
        printfn "send an email to %s" email

    let sendEmail2 (email:EmailAddress) = 
        printfn "send an email to %A" email
    
    let aliceemail = EmailAddress "alice@example.com"
    sendEmail aliceemail

    [<Measure>]
    type cm

    [<Measure>]
    type inches

    [<Measure>]
    type feet =
        static member toInches(feet:float<feet>) : float<inches> =
            feet * 12.0<inches/feet>

    let meter = 100.0<cm>
    let yard = 3.0<feet>

    let yeardInInches = feet.toInches(yard)

    [<Measure>]
    type GBP

    [<Measure>]
    type USD

    let gbp10 =  10.0<GBP>
    let usd10 = 10.0<USD>

    gbp10 + 1.0<GBP>
    gbp10 + 1.0<_>
