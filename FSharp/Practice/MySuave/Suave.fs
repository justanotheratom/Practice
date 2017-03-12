namespace Suave

module Http =

    type RequestType = GET | POST

    type Request = {
        Route: string
        Type: RequestType
    }

    type Response = {
        Content: string
        StatusCode: int
    }

    type Context = {
        Request: Request
        Response: Response
    }

    type WebPart = Context -> Async<Context Option>

module Successful =

    open Http

    // Note: This is a WebPart that takes a string and returns a WebPart.
    // string -> Context -> Async<Context Option>
    // string -> WebPart
    let OK content context =
        { context with Response = { Content = content; StatusCode = 200 } }
        |> Some
        |> async.Return

module Console =

    open Http

    let execute inputContext webPart =
        async {
            let! outputContext = webPart inputContext
            match outputContext with
            | Some context ->
                printfn "--------------------"
                printfn "Code : %d" context.Response.StatusCode
                printfn "Output : %s" context.Response.Content
                printfn "--------------------"
            | None ->
                printfn "No Output"
        }
        |> Async.RunSynchronously