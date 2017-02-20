
//------------------------------------------------------------------------------
// GCD
//------------------------------------------------------------------------------

let rec gcd a b =
    match (b = 0) with
    | true  -> a
    | false -> gcd b (a % b)

gcd 6 9
gcd 2 3

//------------------------------------------------------------------------------
// Newton's method - for finding square root
//------------------------------------------------------------------------------

let sqrt (x : float) =

    let isGoodEnough guess =
        abs(guess * guess - x) / x < 0.001
    
    let improve guess =
        (guess + x / guess) / 2.0
    
    let rec loop guess =
        match isGoodEnough guess with
        | true  -> guess
        | false -> loop (improve guess)
    
    loop 1.0

sqrt 4.
sqrt 2.

//------------------------------------------------------------------------------
