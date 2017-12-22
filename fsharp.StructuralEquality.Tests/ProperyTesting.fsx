#r @"bin/Debug/FsCheck.dll"

open FsCheck

module ProperyTesting=
    let appendedListLength l1 l2=
        (l1 @ l2).Length=l1.Length+l2.Length


    Check.Quick appendedListLength

