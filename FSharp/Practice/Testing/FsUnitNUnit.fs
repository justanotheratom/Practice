module FsUnitNUnit

//
// + Double-backtick syntax.
// + Can have standalone test functions.
// + 'should equal ..' instead of 'Assert.AreEquals'.
//
// - Creating parameterized tests is a pain. (Fuchu looks better).
//

open NUnit.Framework
open FsUnit

[<TestFixture>]
type TestClass () =

    [<Test>]
    member x.``Test in a class`` () =
        2 + 2 |> should equal 4

    [<Test>]
    member x.``Test 2 in a class``() =
        2 + 3 |> should equal 5

[<Test>]
let ``Standalone Test a Function`` () =
    2 + 2 |> should equal 4