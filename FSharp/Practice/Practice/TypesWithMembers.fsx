
//------------------------------------------------------------------------------
// Record with members
//------------------------------------------------------------------------------

type Vector2D =
    { DX: float; DY: float }

    member v.Length         = sqrt(v.DX * v.DX + v.DY * v.DY)
    member v.Scale k        = { DX = k * v.DX; DY = k * v.DY }
    member v.ShiftX x       = { v with DX = v.DX + x }
    member v.ShiftY y       = { v with DY = v.DY + y }
    member v.ShiftXY (x, y) = { DX = v.DX + x; DY = v.DY + y }
    
    // LL: Members with curried arguments cannot be overloaded.
    // member v.ShiftXY x y    = { DX = v.DX + x; DY = v.DY + y }

    static member Zero      = { DX = 0.0; DY = 0.0 }
    static member ConstX dx = { DX = dx;  DY = 0.0 }
    static member ConstY dy = { DX = 0.0; DY = dy  }

    // This declaration element is not permitted in an augmentation.
    // static let zero         = { DX = 0.0; DY = 0.0 }

let v = { DX = 3.0; DY = 4.0 }

v.Scale 3.0
v.Scale(3.0)
v.Length
v.Scale(2.0).Length
Vector2D.Zero
System.Object.ReferenceEquals(Vector2D.Zero, Vector2D.Zero)

//------------------------------------------------------------------------------
// Discriminated Union with members
//------------------------------------------------------------------------------

type Tree<'T> =
    | None
    | Tip of 'T
    | Tree of 'T * Tree<'T> * Tree<'T>

    member t.Size =
        match t with
        | None           -> 0
        | Tip _          -> 1
        | Tree (x, l, r) -> 1 + l.Size + r.Size

//------------------------------------------------------------------------------
// Class type
//------------------------------------------------------------------------------

type Vector2D(dx: float, dy: float) =

    let len = sqrt (dx * dx + dy * dy)

    static let zero = Vector2D(dx = 0.0, dy = 0.0)
    static let oneX = Vector2D(dx = 1.0, dy = 0.0)
    static let oneY = Vector2D(dx = 0.0, dy = 1.0)

    member v.DX     = dx
    member v.DY     = dy
    member v.Length = len

    member v.Scale(k) = Vector2D(dx * k, dy * k)
    member v.ShiftX(x) = Vector2D(dx + x, dy)
    member v.ShiftY(y) = Vector2D(dx, dy + y)
    member v.ShiftXY(x, y) = Vector2D(dx + x, dy + y)

    static member Zero = zero
    static member OneX = oneX
    static member OneY = oneY

let v = Vector2D(3.0, 4.0)
v.Length
v.Scale(2.0).Length

