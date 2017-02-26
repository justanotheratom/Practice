
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------

open System.Collections.Generic

type INameLookupService =
    abstract Contains : string -> bool

let buildSimpleNameLookup (words: string list) =
    let wordTable = HashSet<_>(words)
    { new INameLookupService with
        member t.Contains w = wordTable.Contains w }

let capitalLookup = buildSimpleNameLookup ["London"; "Paris"; "Warsaw"; "Tokyo"]

capitalLookup.Contains "foo"
capitalLookup.Contains "Warsaw"

//------------------------------------------------------------------------------

let isWord (words: string list) =
    let wordTable = words |> Set.ofList
    fun w -> wordTable.Contains(w)

let isCapital = isWord ["London"; "Paris"; "Warsaw"; "Tokyo"]

isCapital "foo"
isCapital "Warsaw"

//------------------------------------------------------------------------------

open System.IO

let tmpFile = Path.Combine(__SOURCE_DIRECTORY__, "temp.txt")

File.WriteAllLines(tmpFile, [|"Line 1"; "Line 2"; "Line 3"; "Something of substance"|])
File.ReadAllLines(tmpFile)
File.ReadAllText(tmpFile)

seq {
    for line in File.ReadLines tmpFile do
        let words = line.Split [|' '|]
        if words.Length > 2 && words.[2] = "substance" then
            yield line
}

//------------------------------------------------------------------------------

open System.Collections.Generic

let capitals = new Dictionary<string, string>(HashIdentity.Structural)

capitals.["USA"]        <- "Washington"
capitals.["Bangladesh"] <- "Dhaka"

// LL: can't say (k, v) instead of kvp
for kvp in capitals do
    printfn "%s has capital %s" kvp.Key kvp.Value

//------------------------------------------------------------------------------

try
    raise (System.InvalidCastException ("don't use casts in vain"))
    raise (System.InvalidOperationException ("this is not allowed"))
with
    | :? System.InvalidOperationException as e -> printfn "Caught: %s" e.Message
    | e -> printfn "Caught unknown exception: %s" e.Message

//------------------------------------------------------------------------------
