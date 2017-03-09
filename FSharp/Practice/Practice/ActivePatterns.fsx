
//------------------------------------------------------------------------------
// Active Patterns as Views for Structured Data
//------------------------------------------------------------------------------

module A =

    type Complex(r: float, i: float) =
        static member Polar(mag, phase) = Complex(mag * cos phase, mag * sin phase)
        member x.Magnitude = sqrt(r * r + i * i)
        member x.Phase = atan2 i r
        member x.RealPart = r
        member x.ImaginaryPart = i

    fsi.AddPrinter (fun (c: Complex) -> sprintf "%gr + %gi" c.RealPart c.ImaginaryPart)

    let (|Rect|) (x: Complex) = (x.RealPart, x.ImaginaryPart)
    let (|Polar|) (x: Complex) = (x.Magnitude, x.Phase)

    let add a b =
        match a, b with
        | Rect (ar, ai), Rect (br, bi) -> Complex (ar + br, ai + bi)

    let multViaRect a b =
        match a, b with
        | Rect (ar, ai), Rect (br, bi) -> Complex (ar * br - ai * bi, ai * br + bi * ar)

    let multViaPolar a b =
        match a, b with
        | Polar (m1, p1), Polar (m2, p2) -> Complex.Polar (m1 * m2, p1 + p2)

    let c = Complex (3.0, 4.0)

    add c c
    multViaRect c c
    multViaPolar c c

    match c with
    | Rect (x, y) -> printfn "x = %g, y = %g" x y

    match c with
    | Polar (m, p) -> printfn "magnitude = %g, phase = %g" m p