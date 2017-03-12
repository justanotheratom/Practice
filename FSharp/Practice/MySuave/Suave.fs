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

module Filters =

    open Http

    let iff condition context =
        if condition context then
            context |> Some |> async.Return
        else
            None |> async.Return

    let GET       = iff (fun c -> c.Request.Type  = GET)
    let POST      = iff (fun c -> c.Request.Type  = POST)
    let Path path = iff (fun c -> c.Request.Route = path)

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

    let parseRequest (input: string) =
        let parts   = input.Split([|';'|])
        let rawType = parts.[0]
        let route   = parts.[1]
        match rawType with
        | "GET"  -> { Type = RequestType.GET;  Route = route }
        | "POST" -> { Type = RequestType.POST; Route = route }
        | _      -> failwith "invalid request"

    let executeInLoop inputContext webPart =
        let mutable continueLooping = true
        while continueLooping do
            printf "Enter Input Route : "
            let input = System.Console.ReadLine()
            try
                if input = "exit" then
                    continueLooping <- false
                else
                    let context = { inputContext with Request = parseRequest input }
                    execute context webPart
            with
                | ex -> printfn "Error : %s" ex.Message
