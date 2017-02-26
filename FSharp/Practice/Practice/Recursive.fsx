
open System
open Microsoft.FSharp.Core

//------------------------------------------------------------------------------

open System.Diagnostics
open System.Collections.Generic

let time f =
    let sw = Stopwatch.StartNew()
    let res = f()
    let finish = sw.Stop()
    (res, sw.Elapsed.TotalMilliseconds |> sprintf "%f ms")

let rec fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib (n - 1) + fib (n - 2)

[1..8]
|> List.map (fun n -> time (fun () -> fib (n * 5)))

let fibFast =
    let t = new Dictionary<int, int>()
    let rec fibCached n =
        match t.ContainsKey n with
        | true  -> t.[n]
        | false ->
            match n with
            | 0 -> 0
            | 1 -> 1
            | _ ->
                let res = fibCached (n - 1) + fibCached (n - 2)
                t.Add (n, res)
                res
    fibCached

[1..8]
|> List.map (fun n -> time (fun () -> fibFast (n * 5)))

time (fun () -> fibFast 10000)

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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