module Program

open Suave.Http
open Suave.Console
open Suave.Successful
open Suave.Filters
open Combinators

[<EntryPoint>]
let main argv =

    let request = { Route = ""; Type = Suave.Http.GET }
    let response = { Content = ""; StatusCode = 200 }
    let context = { Request = request; Response = response }

    let routes = (GET >=> Path "/hello" >=> OK "hello")

    executeInLoop context routes

    0