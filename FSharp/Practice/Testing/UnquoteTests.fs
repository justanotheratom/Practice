module UnquoteTests

open NUnit.Framework
open Swensen.Unquote
open Microsoft.FSharp.Linq.NullableOperators

[<Test>]
let ``When 2 is added to 2 expect 4``() = 
    test <@ 2 + 2 = 4 @>

[<Test>]
let ``When 2.0 + 2.0 result is 4.0 approximately``() = 
    test 
        <@ 
        let result = 2.0 + 2.0
        result < 4.01 && result > 3.99
        @>

(*
// Getting build error here.
[<Test>]
let ``2 + 2 is 4``() = 
   let result = 2 + 2
   result =? 4

[<Test>]
let ``2 + 2 is bigger than 5``() = 
   let result = 2 + 2
   result >? 5
*)