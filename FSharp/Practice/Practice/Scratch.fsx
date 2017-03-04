
//------------------------------------------------------------------------------
// LF: Modules can be extended with functions.
//------------------------------------------------------------------------------

module List =
    let rec pairwise l =
        match l with
        | []            -> []
        | [h]           -> [(h,h)]
        | h1 :: h2 :: t -> (h1, h2) :: (pairwise t)

List.pairwise [1;2;3;4;5;6]
List.pairwise [1;2;3;4;5]
