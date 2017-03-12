module Program

open Suave.Http
open Suave.Console
open Suave.Successful

[<EntryPoint>]
let main argv =

    let request = { Route = ""; Type = Suave.Http.GET }
    let response = { Content = ""; StatusCode = 200 }
    let context = { Request = request; Response = response }

    execute context (OK "Hello Suave!")

    0