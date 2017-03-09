
//------------------------------------------------------------------------------
// Generating and manipulating sequences
//------------------------------------------------------------------------------

module Z =

    seq { 0 .. 10 }
    seq { 0 .. -2 .. -10 }
    seq { 0 .. 2 .. 10 }
    seq { -100 .. 100 }

    let r = seq { 0 .. 2 .. 20 }
    r |> Seq.toList
    r |> Seq.toArray
    for i in r do printfn "%d" i
    r |> Seq.iter (fun n -> printfn "%d" n)
    r |> Seq.map (fun n -> (n, n * n))
    r |> Seq.filter (fun n -> n % 5 = 0)
    r |> Seq.choose (fun n -> if (n % 5 = 0) then Some (n / 5) else None)
    r |> Seq.truncate 2 |> Seq.toArray

    let squares = seq { for i in 0 .. 10 -> (i, i * i) }
    seq { for (i, isquared) in squares -> (i, isquared, i * isquared) }

    let checkerBoardCoordinates n =
        seq { for row in 1 .. n do
                for column in 1 .. n do
                    let sum = row + column
                    if (sum % 2 = 0) then yield (row, column) }

    checkerBoardCoordinates 5 |> Seq.toList

module Y =

    let people =
        [("Amber", 27, "Design")
         ("Wendy", 35, "Events")
         ("Antonio", 40, "Sales")
         ("Petra", 31, "Design")
         ("Carlos", 34, "Marketing")]

    let namesOfPeopleStartingWithA =
        people
        |> Seq.map (fun (n, _, _) -> n)
        |> Seq.filter (fun n -> n.StartsWith "A")
        |> Seq.toList

    let namesOfDesigners =
        people
        |> Seq.filter (fun (_, _, p) -> p = "Design")
        |> Seq.map (fun (n, _, _) -> n)
        |> Seq.toList

module X =

    let rand = System.Random()

    let randomNumbers = seq { while true do yield rand.Next(100000) }

    let first10RandomNumbers =
        randomNumbers
        |> Seq.truncate 10
        |> Seq.sort
        |> Seq.toList

    let first10EvenRandomNumbersWithSquares =
        randomNumbers
        |> Seq.filter (fun n -> n % 2 = 0)
        |> Seq.truncate 10
        |> Seq.sort
        |> Seq.map (fun n -> (n, n * n))
        |> Seq.toList

    let first10RandomNumbersSortedByLastDigit =
        randomNumbers
        |> Seq.truncate 10
        |> Seq.sortBy (fun n -> n % 10)
        |> Seq.toList

module W =

    let triangleNumbers =
        seq { 1 .. 10 }
        |> Seq.collect (fun n -> seq { 1 .. n })
        |> Seq.toList

    let triangleNumbers2 =
        seq { for i in 1 .. 10 do yield! seq { 1 .. i }}
        |> Seq.toList

module V =

    let rand = System.Random()

    let gameBoard =
        seq { for i in 1 .. 7 do
                for j in 1 .. 7 do
                    yield (i, j, rand.Next(10))}

    let evenPositions =
        gameBoard
        |> Seq.choose (fun (i, j, v) -> if (v % 2 = 0) then Some (i, j) else None)
        |> Seq.toList

    let firstElementScoringZero =
        gameBoard
        |> Seq.tryFind (fun (i, j, v) -> v = 0)

    let firstPositionScoringZero =
        gameBoard
        |> Seq.tryPick (fun (i, j, v) -> if (v = 0) then Some (i, j) else None)

//------------------------------------------------------------------------------
// Lazy Sequences
//------------------------------------------------------------------------------

open System
open System.IO

module A =

    let rec allFiles dir =
        Seq.append
            (dir |> Directory.GetFiles)
            (dir |> Directory.GetDirectories |> Seq.collect allFiles)

    let tmpFiles = allFiles (Path.GetTempPath())

    tmpFiles
    |> Seq.truncate 10
    |> Seq.iter (printfn "%A")

module B =

    let rec allFiles dir =
        seq { for f in Directory.GetFiles dir do yield f
              for d in Directory.GetDirectories dir do yield! allFiles d }

    let tmpFiles = allFiles (Path.GetTempPath())

    tmpFiles
    |> Seq.truncate 10
    |> Seq.iter (printfn "%A")

module C =

    type FileInfo =
        { file: string; creationTime: DateTime; lastAccessTime: DateTime }

    let getFileInfo dir =
        seq { for f in Directory.GetFiles dir do
                yield { file = f;
                        creationTime = File.GetCreationTime f;
                        lastAccessTime = File.GetLastAccessTime f } }

    let tmpFileInfo = getFileInfo (Path.GetTempPath())

    tmpFileInfo
    |> Seq.truncate 10
    |> Seq.iter (printfn "%A")

