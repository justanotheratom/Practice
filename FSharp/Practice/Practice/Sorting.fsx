
let rec quicksort list =
    match list with
    | [] -> []
    | first::rest ->
        let smaller =
            rest
            |> List.filter (fun x -> x < first)
            |> quicksort
        let larger =
            rest
            |> List.filter (fun x -> x >= first)
            |> quicksort
        List.concat [smaller; [first]; larger]

quicksort [5;4;3;2;6;7;8]

let rec quicksort2 list =
    match list with
    | [] -> []
    | first::rest ->
        let sortedSubList f = rest |> List.filter f |> quicksort2
        let smaller = sortedSubList (fun x -> x < first)
        let larger = sortedSubList (fun x -> x >= first)
        smaller @ [first] @ larger

quicksort2 [5;4;3;2;6;7;8]

let rec quicksort3 = function
    | [] -> []
    | first::rest ->
        let (smaller,larger) = List.partition ((>=) first) rest
        quicksort3 smaller @ [first] @ quicksort3 larger

quicksort3 [5;4;3;2;6;7;8]

let rec insertionSort xs =

    let rec insert x xs =
        match xs with
        | [] -> [x]
        | y :: ys -> if (x <= y) then x :: xs else y :: insert x ys

    match xs with
    | [] -> []
    | y :: ys -> insert y (insertionSort ys)

insertionSort [5;4;3;2;6;7;8]