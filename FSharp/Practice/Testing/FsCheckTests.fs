module FsCheckTests

// Scott Wlaschin:
// Property based testing is especially useful for "algorithmic" code
// If you reverse a list and then reverse it again, you get the original list.
// If you factorize an integer and then multiply the factors, you get the original number.

open NUnit.Framework
open FsCheck

let revRevIsOrig (xs: list<int>) = List.rev (List.rev xs) = xs

type ListProperties =
  static member ``reverse of reverse is original`` (xs:list<int>) = List.rev(List.rev xs) = xs
  static member ``reverse is original`` (xs:list<int>) = List.rev xs = xs

module UsingFsUnit =

    open FsUnit

    [<Test>]
    let revRevTest () = Check.Quick revRevIsOrig

module UsingFuchu =

    open Fuchu

    [<Tests>]
    let tests =
        testList "FsCheckTests" [
            testCase "List.rev" (fun _ -> Check.Quick revRevIsOrig)
            testCase "List properties" (fun _ -> Check.QuickAll<ListProperties>())
        ]

module UsingFsCheck =

    open FsCheck.NUnit

    [<Property>]
    let ``Reverse of reverse of a list is the original list ``(xs:list<int>) =
      List.rev(List.rev xs) = xs