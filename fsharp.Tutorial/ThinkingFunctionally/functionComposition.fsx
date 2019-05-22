module composition =
    let add1 x = x+1
    let times2 x = x * 2

    //old style
    let add1Times2 x = times2(add1 x)

    //new style
    let add1Times22 = add1 >> times2


