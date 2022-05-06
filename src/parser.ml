open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  match lookahead toks with
  |Some Tok_Let -> parse_let toks
  |Some Tok_If -> parse_if toks
  |Some Tok_Fun -> parse_function toks
  |Some Tok_Not -> parse_or toks
  |Some Tok_Int x -> parse_or toks
  |Some Tok_Bool x -> parse_or toks
  |Some Tok_ID x -> parse_or toks
  |Some Tok_LParen -> parse_or toks
  |Some Tok_String x -> parse_or toks
  |_ -> raise (InvalidInputException "parse_expr")

and parse_primary toks =
  match lookahead toks with
  |Some Tok_Int x -> (let rest = match_token toks (Tok_Int x) in 
    (rest, Value (Int x)))
  |Some Tok_Bool x -> let rest = match_token toks (Tok_Bool x) in 
    (rest, Value (Bool x))
  |Some Tok_ID x -> let rest = match_token toks (Tok_ID x) in 
    (rest, ID x)
  |Some Tok_String x -> let rest = match_token toks (Tok_String x) in 
    (rest, Value (String x))
  |Some Tok_LParen -> let rest = match_token toks Tok_LParen in
    let (rest2, expr) = parse_expr rest in
    let rest3 = match_token rest2 Tok_RParen in
    (rest3, expr)
  |_ -> raise (InvalidInputException "parse_primary")

  and parse_functionCall toks = 
    let (rest, expr) = parse_primary toks in
    match lookahead rest with
    |Some Tok_Int x -> (let (rest2, expr2) = parse_primary rest in
      (rest2, FunctionCall(expr, expr2)))
    |Some Tok_Bool x -> (let (rest2, expr2) = parse_primary rest in
      (rest2, FunctionCall(expr, expr2)))
    |Some Tok_ID x -> (let (rest2, expr2) = parse_primary rest in
      (rest2, FunctionCall(expr, expr2)))
    |Some Tok_LParen -> (let (rest2, expr2) = parse_primary rest in
      (rest2, FunctionCall(expr, expr2)))
    |_ -> (rest, expr)

  and parse_unary toks =
    match lookahead toks with
    |Some Tok_Not -> (let rest = match_token toks Tok_Not in
      let (rest2, expr) = parse_unary rest in
      (rest2, Not (expr)))
    |Some Tok_Int x -> parse_functionCall toks
    |Some Tok_Bool x -> parse_functionCall toks
    |Some Tok_ID x -> parse_functionCall toks
    |Some Tok_String x -> parse_functionCall toks
    |Some Tok_LParen -> parse_functionCall toks
    |_ -> raise (InvalidInputException "parse_unary")

  and parse_concat toks =
    let (rest, expr) = parse_unary toks in
    match lookahead rest with
    |Some Tok_Concat -> (let rest2 = match_token rest Tok_Concat in
      let (rest3, expr2) = parse_concat rest2 in
      (rest3, Binop (Concat, expr, expr2)))
    |_ -> (rest, expr)

  and parse_mult toks =
    let (rest, expr) = parse_concat toks in
    match lookahead rest with
    |Some Tok_Mult -> (let rest2 = match_token rest Tok_Mult in
      let (rest3, expr2) = parse_mult rest2 in
      (rest3, Binop (Mult, expr, expr2)))
    |Some Tok_Div -> (let rest2 = match_token rest Tok_Div in
    let (rest3, expr2) = parse_mult rest2 in
    (rest3, Binop (Div, expr, expr2)))
    |_ -> (rest, expr)

  and parse_add toks = 
    let (rest, expr) = parse_mult toks in
    match lookahead rest with
    |Some Tok_Add -> (let rest2 = match_token rest Tok_Add in
      let (rest3, expr2) = parse_add rest2 in
      (rest3, Binop (Add, expr, expr2)))
    |Some Tok_Sub -> (let rest2 = match_token rest Tok_Sub in
      let (rest3, expr2) = parse_add rest2 in
      (rest3, Binop (Sub, expr, expr2)))
    |_ -> (rest, expr)

  and parse_relational toks = 
    let (rest, expr) = parse_add toks in
    match lookahead rest with
    |Some Tok_Less -> (let rest2 = match_token rest Tok_Less in
      let (rest3, expr2) = parse_relational rest2 in
      (rest3, Binop (Less, expr, expr2)))
    |Some Tok_Greater -> (let rest2 = match_token rest Tok_Greater in
      let (rest3, expr2) = parse_relational rest2 in
      (rest3, Binop (Greater, expr, expr2)))
    |Some Tok_LessEqual -> (let rest2 = match_token rest Tok_LessEqual in
      let (rest3, expr2) = parse_relational rest2 in
      (rest3, Binop (LessEqual, expr, expr2)))
    |Some Tok_GreaterEqual -> (let rest2 = match_token rest Tok_GreaterEqual in
      let (rest3, expr2) = parse_relational rest2 in
      (rest3, Binop (GreaterEqual, expr, expr2)))
    |_ -> (rest, expr)

  and parse_equality toks = 
    let (rest, expr) = parse_relational toks in
    match lookahead rest with
    |Some Tok_Equal -> (let rest2 = match_token rest Tok_Equal in
      let (rest3, expr2) = parse_equality rest2 in
      (rest3, Binop (Equal, expr, expr2)))
    |Some Tok_NotEqual -> (let rest2 = match_token rest Tok_NotEqual in
      let (rest3, expr2) = parse_equality rest2 in
      (rest3, Binop (NotEqual, expr, expr2)))
    |_ -> (rest, expr)

  and parse_and toks = 
    let (rest, expr) = parse_equality toks in
    match lookahead rest with
    |Some Tok_And -> (let rest2 = match_token rest Tok_And in
      let (rest3, expr2) = parse_and rest2 in
      (rest3, Binop (And, expr, expr2)))
    |_ -> (rest, expr)

  and parse_or toks = 
    let (rest, expr) = parse_and toks in
    match lookahead rest with
    |Some Tok_Or -> (let rest2 = match_token rest Tok_Or in
      let (rest3, expr2) = parse_or rest2 in
      (rest3, Binop (Or, expr, expr2)))
    |_ -> (rest, expr)

  and parse_if toks = 
    match lookahead toks with
    |Some Tok_If -> (let rest = match_token toks Tok_If in
      let (rest2, expr) = parse_expr rest in
      match lookahead rest2 with
      |Some Tok_Then -> (let rest3 = match_token rest2 Tok_Then in
        let (rest4, expr2) = parse_expr rest3 in
        match lookahead rest4 with
        |Some Tok_Else -> (let rest5 = match_token rest4 Tok_Else in
          let (rest6, expr3) = parse_expr rest5 in
          (rest6, If (expr, expr2, expr3))
        )
        |_ -> raise (InvalidInputException "pase_else")
      )
      |_ -> raise (InvalidInputException "parse_then")
    )
    |_ -> raise (InvalidInputException "parse_if")

  and parse_function toks = 
    match lookahead toks with
    |Some Tok_Fun -> (let rest = match_token toks Tok_Fun in
      match lookahead rest with
      |Some Tok_ID x -> (let rest2 = match_token rest (Tok_ID x) in
        match lookahead rest2 with
        |Some Tok_Arrow -> (let rest3 = match_token rest2 Tok_Arrow in
          let (rest4, expr) = parse_expr rest3 in
          (rest4, Fun (x, expr))
        )
        |_ -> raise (InvalidInputException "parse_fun")
      )
      |_ -> raise (InvalidInputException "parse_fun")
    )
    |_ -> raise (InvalidInputException "parse_fun")

  and parse_let toks =  
    match lookahead toks with
    |Some Tok_Let -> (let rest = match_token toks Tok_Let in

      match lookahead rest with

      |Some (Tok_ID x) -> (let rest2 = match_token rest (Tok_ID x) in
        match lookahead rest2 with
        |Some Tok_Equal -> (let rest3 = match_token rest2 Tok_Equal in
          let (rest4, expr) = parse_expr rest3 in
          match lookahead rest4 with
          |Some Tok_In -> (let rest5 = match_token rest4 Tok_In in
          let (rest6, expr2) = parse_expr rest5 in
          (rest6, Let(x, false, expr, expr2)))
          |_ -> raise (InvalidInputException "parse_let fun")
          )
        |_ -> raise (InvalidInputException "parse_let fun")
      )
      |Some Tok_Rec -> (let rest2 = match_token rest Tok_Rec in
        match lookahead rest2 with
        |Some (Tok_ID x) -> (let rest3 = match_token rest2 (Tok_ID x) in
          match lookahead rest3 with
          |Some Tok_Equal -> (let rest4 = match_token rest3 Tok_Equal in
            let (rest5, expr) = parse_expr rest4 in
            match lookahead rest5 with
            |Some Tok_In -> (let rest6 = match_token rest5 Tok_In in
              let (rest7, expr2) = parse_expr rest6 in
              (rest7, Let(x, true, expr, expr2)))
            |_ -> raise (InvalidInputException "parse_let rec fun")
            )
          |_ -> raise (InvalidInputException "parse_let rec fun")
          ) 
        |_ -> raise (InvalidInputException "parse_let rec fun")
      )
      |_ -> raise (InvalidInputException "parse_let fun")
    )
    |_ -> raise (InvalidInputException "parse_let")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  |Some Tok_Def -> parse_mutop_def toks
  |Some Tok_DoubleSemi -> ((match_token toks Tok_DoubleSemi), NoOp)
  |Some Tok_If -> parse_mutop_expr toks
  |Some Tok_Let -> parse_mutop_expr toks
  |Some Tok_Fun -> parse_mutop_expr toks
  |Some Tok_Not -> parse_mutop_expr toks
  |Some Tok_Bool x -> parse_mutop_expr toks
  |Some Tok_Int x -> parse_mutop_expr toks
  |Some Tok_String x -> parse_mutop_expr toks
  |Some Tok_ID x -> parse_mutop_expr toks
  |Some Tok_LParen -> parse_mutop_expr toks
  |_ -> raise (InvalidInputException "parse_mutop")

and parse_mutop_expr toks =
  let (toks2, x) = parse_expr toks in
  ((match_token toks2 Tok_DoubleSemi), Expr x)

and parse_mutop_def toks = 
  match lookahead_many toks 1 with
  |Some Tok_ID y -> (let (toks2, x) = parse_expr(match_many toks [Tok_Def; Tok_ID y; Tok_Equal]) in
    ((match_token toks2 Tok_DoubleSemi), Def(y, x)))
  |_ -> raise (InvalidInputException "parse_mutop_def")