module FuchuTest

//
// + Can do parameterized tests.
// + Flexible grouping of tests.
// + Flexible running of tests.
// + Can be mixed with FsUnit - 'should equal'
//

open NUnit.Framework
open Fuchu
open FsUnit

let add1 x = if (x % 9 <> 0) then x + 1 else x    

let ``Assert that add1 is x+1`` x _notUsed = 
   add1 x |> should equal (x + 1)

let simpleTest = 
   testCase "Test with 42" <| ``Assert that add1 is x+1`` 42

let parameterizedTest i = 
   testCase (sprintf "Test with %i" i) <| ``Assert that add1 is x+1`` i

[<Tests>]
let tests = 
   testList "Test group A" [
      simpleTest 
      testList "Parameterized 9..10" ([9..10] |> List.map parameterizedTest) 
   ]