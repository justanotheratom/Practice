
//------------------------------------------------------------------------------
// Marshall/Unmarshall to/from binary
//------------------------------------------------------------------------------

[<AutoOpen>]
module Marshaller =

    type OutState = System.IO.BinaryWriter
    type InState = System.IO.BinaryReader

    type Pickler<'T> = 'T -> OutState -> unit
    type Unpickler<'T> = InState -> 'T

    let byteP (b: byte) (st: OutState) = st.Write(b)
    let byteU (st: InState) = st.ReadByte()

    let boolP b (st: OutState) = byteP (if (b) then 1uy else 0uy) st
    let boolU (st: InState) = let b = byteU st in (b = 1uy)

    let int32P i (st: OutState) =
        byteP (byte (i &&& 0xFF)) st
        byteP (byte ((i >>> 8) &&& 0xFF)) st
        byteP (byte ((i >>> 16) &&& 0xFF)) st
        byteP (byte ((i >>> 24) &&& 0xFF)) st
    
    let int32U (st: InState) =
        let b0 = int (byteU st)
        let b1 = int (byteU st)
        let b2 = int (byteU st)
        let b3 = int (byteU st)
        b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24)

    let tup2P p1 p2 (a, b) (st: OutState) =
        (p1 a st : unit)
        (p2 b st : unit)

    let tup3P p1 p2 p3 (a, b, c) (st: OutState) =
        (p1 a st : unit)
        (p2 b st : unit)
        (p3 c st : unit)

    let tup2U u1 u2 (st: InState) =
        let a = u1 st
        let b = u2 st
        (a, b)

    let tup3U u1 u2 u3 (st: InState) =
        let a = u1 st
        let b = u2 st
        let c = u3 st
        (a, b, c)

    let rec listP f lst (st: OutState) =
        match lst with
        | [] -> byteP 0uy st
        | h :: t ->
            byteP 1uy st
            f h st
            listP f t st

    let listU u (st: InState) =
        let rec loop acc =
            let tag = byteU st
            match tag with
            | 0uy -> List.rev acc
            | 1uy -> let a = u st in loop (a :: acc)
            | n -> failwithf "listU: unexpected number %d" n
        loop []

module Test =

    open System.IO

    type Format = list<int32 * bool>

    let formatP (l: Format) = listP (tup2P int32P boolP) l
    let formatU (st: InState) = listU (tup2U int32U boolU) st

    let writeData file data =
        use outStream = new BinaryWriter(File.OpenWrite(file))
        formatP data outStream

    let readData file =
        use inputStream = new BinaryReader(File.OpenRead(file))
        formatU inputStream

    let tempFile = Path.GetTempFileName()

    writeData tempFile [(101, false); (93, true)]

    readData tempFile
