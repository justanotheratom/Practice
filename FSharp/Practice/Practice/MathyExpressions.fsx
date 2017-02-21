
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

let rec simplify e =
    match e with
    | Sum (Product (a, b), Product (c, d)) ->
        if (a = c)      then simplify (Product (a, Sum (b, d)))
        else if (a = d) then simplify (Product (a, Sum (b, c)))
        else if (b = c) then simplify (Product (b, Sum (a, d)))
        else if (b = d) then simplify (Product (b, Sum (a, c)))
        else e
    | Product (l, r) -> Product (simplify l, simplify r)
    | Sum (l, r)     -> Sum (simplify l, simplify r)
    | e -> e

let parse (e: string) =
    exn


let s = Sum (Number 1, Number 2)
show s
eval s

let p = Product (Number 1, Number 2)
show p
eval p

let c1 = Sum (Product (Number 2, Number 3), Product (Number 2, Number 4))
show (simplify c1) = "(2 * (3 + 4))"
eval c1 = eval (simplify c1)
