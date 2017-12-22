module Query=
    type Person={
        Name:string
        Age:int
    }

    let data=[
        {Name="tom";Age=22}
        {Name="tom1";Age=22}
        {Name="tom2";Age=22}
        {Name="tom3";Age=22}
        {Name="tom4";Age=22}
    ]

    //F# collection functions
    let namesOfAdults=List.filter(fun p->p.Age>18) data
                    |>List.map(fun p->p.Name)

    let namesOfAdults'=data
                     |>List.filter (fun p->p.Age>18)
                     |>List.map(fun p->p.Name)

    

    //LINQ extension methods
    open System.Linq
    let namesOfAdultsLinq=
        data.Where(fun p->p.Age>18)
            .Select(fun p->p.Name)

    printfn "%A" namesOfAdultsLinq

    //Linq expression
    let namesOfAdultsLinqQuery=
        query{
            for p in data do
             where (p.Age>18)
             select p.Name
        }

    printfn "%A" namesOfAdultsLinqQuery
