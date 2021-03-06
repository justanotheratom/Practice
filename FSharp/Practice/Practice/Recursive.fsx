﻿
open System
open Microsoft.FSharp.Core

//------------------------------------------------------------------------------
// Type definitions can be recursive too!
//------------------------------------------------------------------------------

type Tree<'T> =
    | None
    | Tip of 'T
    | Tree of 'T * Tree<'T> * Tree<'T>

let rec size tree =
    match tree with
    | None           -> 0
    | Tip _          -> 1
    | Tree (_, l, r) -> 1 + (size l) + (size r)

let rec cataTree fNone fTip fTree tree =
    let recurse = cataTree fNone fTip fTree
    match tree with
    | None           -> fNone ()
    | Tip t          -> fTip t
    | Tree (t, l, r) -> fTree t (recurse l) (recurse r)

let size2 tree =
    let fNone () = 0
    let fTip t = 1
    let fTree t l r = 1 + l + r
    cataTree fNone fTip fTree tree

let print tree =
    let fNone () = "None"
    let fTip t = t.ToString()
    let fTree t l r = "(" + t.ToString() + " => " + l + ", " + r + ")"
    cataTree fNone fTip fTree tree

let deepCopy tree =
    let fNone () = None
    let fTip t = Tip t
    let fTree t l r = Tree (t, l, r)
    cataTree fNone fTip fTree tree

let smallTree = Tree("1", Tree("2", Tip "a", Tip "b"), None)

size smallTree
size2 smallTree
print smallTree

let copyTree = deepCopy smallTree
System.Object.ReferenceEquals(smallTree, copyTree) = false

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
        if t.ContainsKey n then t.[n]
        else
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

let memoize (f: 'T -> 'U) =
    let t = new Dictionary<'T, 'U>(HashIdentity.Structural)
    fun x ->
        if t.ContainsKey x then t.[x]
        else
            let res = f x
            t.Add (x, res)
            res

#nowarn "40"

let rec fibFast2 =
    memoize (fun n ->
        match n with
        | 0 -> 0
        | 1 -> 1
        | _ -> fibFast2 (n - 1) + fibFast2 (n - 2)
    )

time (fun () -> fibFast2 10000)

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