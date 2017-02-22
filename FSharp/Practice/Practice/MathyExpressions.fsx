
// TODO:
// [] Parser for expressions in string format.
// [] Property-based tests.
// [] More simplifications of expressions.

type Expr =
    | Number  of int
    | Sum     of Expr * Expr
    | Product of Expr * Expr

let rec eval =
    function
    | Number n       -> n
    | Sum (l, r)     -> eval l + eval r
    | Product (l, r) -> eval l * eval r

let rec show =
    function
    | Number n       -> n.ToString()
    | Sum (l, r)     -> "(" + show l + " + " + show r + ")"
    | Product (l, r) -> "(" + show l + " * " + show r + ")"

//------------------------------------------------------------------------------

let rec simplify_1 e =
    match e with
    | Sum (Product (a, b), Product (c, d)) ->
        if (a = c)      then simplify_1 <| Product (a, Sum (b, d))
        else if (a = d) then simplify_1 <| Product (a, Sum (b, c))
        else if (b = c) then simplify_1 <| Product (b, Sum (a, d))
        else if (b = d) then simplify_1 <| Product (b, Sum (a, c))
        else e
    | Product (l, r) -> Product (simplify_1 l, simplify_1 r)
    | Sum (l, r)     -> Sum (simplify_1 l, simplify_1 r)
    | e -> e
    // LL: can't say
    // | Sum (Product (a, b), Product (a, d)) -> simplify_1 <| Product (a, Sum (b, d))

//------------------------------------------------------------------------------

let rec simplify_2 =
    function
    | Sum (Product (a, b), Product (c, d)) when (a = c) -> simplify_2 <| Product (a, Sum (b, d))
    | Sum (Product (a, b), Product (c, d)) when (a = d) -> simplify_2 <| Product (a, Sum (b, c))
    | Sum (Product (a, b), Product (c, d)) when (b = c) -> simplify_2 <| Product (b, Sum (a, d))
    | Sum (Product (a, b), Product (c, d)) when (b = d) -> simplify_2 <| Product (b, Sum (a, c))
    | Sum (l, r)     -> Sum (simplify_2 l, simplify_2 r)
    | Product (l, r) -> Product (simplify_2 l, simplify_2 r)
    | Number _ as n  -> n
    | _ -> failwith "invalid input"

//------------------------------------------------------------------------------

let (|CommonFactor|_|) =
    function
    | Sum (Product (a, b), Product (c, d)) when (a = c) -> Some <| Product (a, Sum (b, d))
    | Sum (Product (a, b), Product (c, d)) when (a = d) -> Some <| Product (a, Sum (b, c))
    | Sum (Product (a, b), Product (c, d)) when (b = c) -> Some <| Product (b, Sum (a, d))
    | Sum (Product (a, b), Product (c, d)) when (b = d) -> Some <| Product (b, Sum (a, c))
    | _ -> None

let (|Operation|_|) simplify = // LL: simplify has to be passed in.
    function
    | Sum (l, r)     -> Some <| Sum (simplify l, simplify r)
    | Product (l, r) -> Some <| Product (simplify l, simplify r)
    | _ -> None

let (|Constant|_|) =
    function
    | Number _ as n -> Some n // LL: can't say 'Number n'.
    | _ -> None

let rec simplify =
    function
    | CommonFactor e -> simplify e
    | Operation simplify e -> e
    | Constant e -> e
    | _ -> failwith "invalid input"

//------------------------------------------------------------------------------

let parse (e: string) =
    exn

//------------------------------------------------------------------------------

let s = Sum (Number 1, Number 2)
show s
eval s

let p = Product (Number 1, Number 2)
show p
eval p

let c1 = Sum (Product (Number 2, Number 3), Product (Number 2, Number 4))
[simplify_1; simplify_2; simplify]
|> List.iter (fun f ->
    do
        printfn "%b" (show (f c1) = "(2 * (3 + 4))")
        printfn "%b" (eval c1 = eval (f c1))
   )
