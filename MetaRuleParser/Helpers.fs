module Helpers

let equalsOn f x (yobj:obj) = 
        match yobj with
        | :? 'T as y -> (f x = f y)
        | _ -> false

let hashOn f x =  hash (f x)

let compareOn f x (yobj: obj) = 
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"

//
//  Sample useage:-
//
//  type stamp = int
//
//  [<CustomEquality; CustomComparison>]
//
//  type MyUnionType = 
//      | MyUnionType of stamp * (int -> int)
//
//      static member Stamp (MyUnionType (s,_)) = s
//      override x.Equals y = equalsOn MyUnionType.Stamp x y
//      override x.GetHashCode() = hashOn MyUnionType.Stamp x
//      interface System.IComparable with
//        member x.CompareTo y = compareOn MyUnionType.Stamp x y
