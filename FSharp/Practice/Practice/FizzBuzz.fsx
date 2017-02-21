
// TODO:
// [] Finish the tests, they are not working.
// [] Write the version that uses Active Pattern.

#r @"..\packages\xunit.abstractions.2.0.0\lib\net35\xunit.abstractions.dll"
#r @"..\packages\xunit.extensibility.core.2.1.0\lib\dotnet\xunit.core.dll"
#r @"..\packages\xunit.extensibility.execution.2.1.0\lib\net45\xunit.execution.desktop.dll"
#r @"..\packages\FsCheck.2.7.1\lib\net45\FsCheck.dll"
#r @"..\packages\FsCheck.Xunit.2.7.1\lib\net45\FsCheck.Xunit.dll"

module FizzBuzz =

    let FizzBuzz number =
        match number % 3, number % 5 with
        | 0, 0 -> "FizzBuzz"
        | _, 0 -> "Buzz"
        | 0, _ -> "Fizz"
        | _, _ -> sprintf "%i" number

    let FizzBuzz2 number =
        match number with
        | _ when number % (5 * 3) = 0 -> "FizzBuzz"
        | _ when number % 5 = 0 -> "Buzz"
        | _ when number % 3 = 0 -> "Fizz"
        | _ -> sprintf "%i" number

module FRP =

    open System
    open System.Threading
    open System.Timers

    type SerializedLogger() =
        static let agent = MailboxProcessor.Start(fun inbox ->
            let rec messageLoop () = async {
                let! msg = inbox.Receive()
                Console.Write (string msg)
                return! messageLoop()
            }
            messageLoop()
        )
        static member Log msg = agent.Post msg

    let createTimerAndObservable timerInterval =
        let timer = new Timer(float timerInterval)
        timer.AutoReset <- true
        let observable = timer.Elapsed
        let task = async {
            timer.Start()
            do! Async.Sleep 5000
            timer.Stop()
        }
        (task, observable)

    type FizzBuzzEvent = {label:int; time:DateTime}

    let areSimultaneous (event1, event2) =
        let {time=t1} = event1
        let {time=t2} = event2
        t2.Subtract(t1).Milliseconds < 50

    let timer3, timerEventStream3 = createTimerAndObservable 300
    let timer5, timerEventStream5 = createTimerAndObservable 500

    let eventStream3 =
        timerEventStream3
        |> Observable.map (fun _ -> {label=3; time=DateTime.Now})

    let eventStream5 =
        timerEventStream5
        |> Observable.map (fun _ -> {label=5; time=DateTime.Now})

    let combinedStream = Observable.merge eventStream3 eventStream5

    let pairwiseStream =
        combinedStream
        |> Observable.pairwise

    let simultaneousStream, nonSimultaneousStream =
        pairwiseStream
        |> Observable.partition areSimultaneous

    let fizzStream, buzzStream =
        nonSimultaneousStream
        |> Observable.map (fun (l,_) -> l)
        |> Observable.partition (fun {label=id} -> id = 3)

    combinedStream
    |> Observable.subscribe
        (fun {label=id;time=t} ->
             SerializedLogger.Log (sprintf "[%i] %i.%03i " id t.Second t.Millisecond))

    fizzStream
    |> Observable.subscribe (fun _ -> SerializedLogger.Log "Fizz\n")

    buzzStream
    |> Observable.subscribe (fun _ -> SerializedLogger.Log "Buzz\n")

    simultaneousStream
    |> Observable.subscribe (fun _ -> SerializedLogger.Log "FizzBuzz\n")

    [timer3;timer5]
    |> Async.Parallel
    |> Async.RunSynchronously

module FizzBuzzTest =

    open FsCheck
    open FsCheck.Xunit

    [<Property>]
    let ``Test`` (number : int) =
        let actual = FizzBuzz.FizzBuzz ((number + 1) * 3)
        let expected = "Fizz"
        expected = actual

    Check.Quick ``Test``