
open System
open Microsoft.FSharp.Core

let rec fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib (n - 1) + fib (n - 2)

let rec factorial n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | _ -> n * factorial (n - 1)

let rec factorial2 n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial2 (n - 1)

let rec factorial3 =
    function
    | 0 | 1 -> 1
    | n -> n * factorial3 (n - 1)

let rec sumList =
    function
    | [] -> 0
    | head::tail -> head + sumList(tail)

let rec aggregateList op init =
    function
    | [] -> init
    | head::tail -> op head (aggregateList op init tail)

// aggregateList (+) 0 [1..5]
// aggregateList (*) 1 [1..5]
// aggregateList max -1 [1 ; 4 ; 8 ; 3 ; 2]
// aggregateList min Int32.MaxValue [1 ; 4 ; 8 ; 3 ; 2]
// aggregateList (fun a b -> String.concat ", " [a; b]) "" ["tomatoes"; "bananas"; "apples"]