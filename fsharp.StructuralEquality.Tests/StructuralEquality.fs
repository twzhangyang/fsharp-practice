namespace fsharp.StructuralEquality.Tests
open System

module message=
    type UserCreated={
        UserId:Guid
        UserName:string
        Password:string
        Email:string
    }

    let user1={
        UserId=Guid "BA8A7326-06DA-4430-B355-6D91C7355CD1"
        UserName="richie"
        Password="password"
        Email="richie@163.com"
    }

    let user2={
        UserId=Guid "BA8A7326-06DA-4430-B355-6D91C7355CD2"
        UserName="richie2"
        Password="password2"
        Email="richie2@163.com"
    }

    user1=user2

    let user2'={
        UserId=Guid "BA8A7326-06DA-4430-B355-6D91C7355CD2"
        UserName="richie2"
        Password="password2"
        Email="richie2@163.com"
    }

    user2=user2'

    type EmailVerified={
        UserId:Guid
        Email:string
    }

    let emailVerified={
        UserId=Guid "BA8A7326-06DA-4430-B355-6D91C7355CD2"
        Email="richie@126.com"
    }

    type UserEvent=
    | UserCreated of UserCreated
    | EmailVerified of EmailVerified

    let userEvent1=UserCreated user1
    let userEvent2=UserCreated user2
    let userEvent2'=UserCreated user2'

    userEvent1=userEvent2
    userEvent2=userEvent2'

    let emailEvent=EmailVerified emailVerified
    userEvent1=emailEvent

    [user1;user2]=[user1;user2]

    [|user1;user2|]=[|user1;user2|]

    let ver1=Version(1,2)
    let ver2=Version(1,2)

    ver1=ver2


