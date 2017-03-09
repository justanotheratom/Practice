
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

    let (|Rect|) (x: Complex) = (x.RealPart, x.ImaginaryPart)
    let (|Polar|) (x: Complex) = (x.Magnitude, x.Phase)

    let c = Complex (3.0, 4.0)

    match c with
    | Rect (x, y) -> printfn "x = %g, y = %g" x y

    match c with
    | Polar (m, p) -> printfn "magnitude = %g, phase = %g" m p