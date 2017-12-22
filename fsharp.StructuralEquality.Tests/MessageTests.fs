namespace fsharp.StructuralEquality.Tests
open System

module Message=
    type envelop<'a>={
        Id:Guid
        Created:DateTimeOffset
        Item:'a
    }

    let envelop getId getTime getItem={
        Id=getId()
        Created=getTime()
        Item=getItem
    }

module MessageTest=
    open Xunit

    type Foo={Text:string;Number:int}

    [<Theory>]
    [<InlineData("7B0EC0F8-B213-46D5-87CA-F095229CDACB", 635575303021425464L, 1., "Bar", 42)>]
    [<InlineData("39E940D6-B820-42BC-8F4D-AE587593DB95", 635575279586114123L, 0., "Baz", 1337)>]
    let ``envelop returns correct results`` (id:string)(ticks:int64)(offset:float)(text:string)(number:int)=
        let getId =fun _->Guid id
        let getTime =fun _->DateTimeOffset(ticks,TimeSpan.FromHours offset)
        let item={Text=text;Number=number}

        let actual=Message.envelop getId getTime item

        Assert.Equal(Guid id,actual.Id)
        Assert.Equal(DateTimeOffset(ticks,TimeSpan.FromHours offset),actual.Created)
        Assert.Equal(item,actual.Item)
    

