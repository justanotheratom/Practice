module FsCheckTests

// Scott Wlaschin:
// Property based testing is especially useful for "algorithmic" code
// If you reverse a list and then reverse it again, you get the original list.
// If you factorize an integer and then multiply the factors, you get the original number.

open NUnit.Framework
open FsCheck
open FsUnit

let revRevIsOrig (xs: list<int>) = List.rev (List.rev xs) = xs

[<Test>]
let listRevTest () = Check.Quick revRevIsOrig