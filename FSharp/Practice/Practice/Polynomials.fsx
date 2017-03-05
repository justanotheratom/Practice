
//------------------------------------------------------------------------------
// Polynomial expression parser
//------------------------------------------------------------------------------

[<AutoOpen>]
module Tokenizer =
    
    type Token =
        | HAT
        | PLUS
        | MINUS
        | INT of int
        | ID of string

    type TokenStream = Token list

    let tokenize (s: string) =

        let regex r = new System.Text.RegularExpressions.Regex(r)

        let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*"

        [for x in tokenR.Match(s).Groups.["token"].Captures do
            let token =
                match x.Value with
                | "^"                              -> HAT
                | "+"                              -> PLUS
                | "-"                              -> MINUS
                | s when System.Char.IsDigit s.[0] -> INT (int s)
                | s                                -> ID s
            yield token]

tokenize "2x^3 + x^2 + 3"

[<AutoOpen>]
module Parser =

    type Operation =
        | PLUS
        | MINUS

    type Term =
        | Term of int * string * int
        | Op of Operation
        | Const of int

    type Polynomial = Term list

    let parse input =

        let getToken tokens =
            match tokens with
            | token :: rest -> Some(token, rest)
            | _ -> None

        let parseExponent tokens =
            match getToken tokens with
            | Some (HAT, tokens) ->
                match getToken tokens with
                | Some (INT i, tokens) -> i, tokens
                | _ -> failwith "expected an integer after '^'"
            | _ -> 1, tokens                    

        let parseTerm tokens =
            match getToken tokens with
            | Some (INT i, tokens) ->
                match getToken tokens with
                | Some (ID id, tokens) ->
                    let (idx, tokens) = parseExponent tokens
                    Term (i, id, idx), tokens
                | _ -> Const i, tokens
            | _ -> failwith "end of token stream in term"

        let rec parseTokens tokens =
            let (t1, rest) = parseTerm tokens
            match getToken rest with
            | Some (Token.PLUS, tokens) ->
                let (p2, rest) = parseTokens tokens
                (t1 :: Op Operation.PLUS :: p2), rest
            | Some (Token.MINUS, tokens) ->
                let (p2, rest) = parseTokens tokens
                (t1 :: Op Operation.MINUS :: p2), rest
            | _ -> [t1], rest

        let (result, rest) = input |> tokenize |> parseTokens
        match getToken rest with
        | Some _ -> failwith "unexpexted input at end of token stream!"
        | None -> result

parse "2x^3 + 3x + 3"
parse "1+3"
parse "1-3"