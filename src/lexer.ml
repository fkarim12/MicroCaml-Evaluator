open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let rec next_token input pos =
    if pos >= (String.length input) then []

    else if Str.string_match (Str.regexp " ") input pos then
      next_token input (pos + 1)

    else if Str.string_match (Str.regexp "\t\\|\n") input pos then
      next_token input (pos + 2)

    else if Str.string_match (Str.regexp ";;") input pos then
      Tok_DoubleSemi::(next_token input (pos + 2))
    
    else if Str.string_match (Str.regexp "\\^") input pos then
      Tok_Concat::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp "let[^a-zA-Z0-9]") input pos then
      Tok_Let::(next_token input (pos + 3))

    else if Str.string_match (Str.regexp "rec") input pos then
      Tok_Rec::(next_token input (pos + 3))

    else if Str.string_match (Str.regexp "in") input pos then
      Tok_In::(next_token input (pos + 3))

    else if Str.string_match (Str.regexp "def\\s") input pos then
      Tok_Def::(next_token input (pos + 4))

    else if Str.string_match (Str.regexp "def") input pos then
      Tok_Def::(next_token input (pos + 3)) 

    else if Str.string_match (Str.regexp "fun") input pos then
      Tok_Fun::(next_token input (pos + 4))

    else if Str.string_match (Str.regexp "->") input pos then
      Tok_Arrow::(next_token input (pos + 2))

    else if Str.string_match (Str.regexp "<>") input pos then
      Tok_NotEqual::(next_token input (pos + 2))

    else if Str.string_match (Str.regexp "<=") input pos then
      Tok_LessEqual::(next_token input (pos + 2))

    else if Str.string_match (Str.regexp ">=") input pos then
      Tok_GreaterEqual::(next_token input (pos + 2))

    else if Str.string_match (Str.regexp "=") input pos then
      Tok_Equal::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp ">") input pos then
      Tok_Greater::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp "<") input pos then
      Tok_Less::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp "&&") input pos then
      Tok_And::(next_token input (pos + 2))

    else if Str.string_match (Str.regexp "||") input pos then
      Tok_Or::(next_token input (pos + 2))

    else if Str.string_match (Str.regexp "not") input pos then
      Tok_Not::(next_token input (pos + 3))

    else if Str.string_match (Str.regexp "if") input pos then
      Tok_If::(next_token input (pos + 3))

    else if Str.string_match (Str.regexp "then") input pos then
      Tok_Then::(next_token input (pos + 5))

    else if Str.string_match (Str.regexp "else") input pos then
      Tok_Else::(next_token input (pos + 5))

    else if Str.string_match (Str.regexp "true\\|false") input pos then
      let matched_bool = Str.matched_string input in
      Tok_Bool (bool_of_string matched_bool)::(next_token input (pos + (String.length matched_bool)))
    
    else if Str.string_match (Str.regexp "(-[0-9]+)\\|[0-9]+") input pos then
      let matched_int = Str.matched_string input in
      if (String.sub matched_int 0 1) = "(" then
        Tok_Int(int_of_string (String.sub matched_int 1 ((String.length matched_int) - 2)))::(next_token input (pos + (String.length matched_int)))
      else Tok_Int (int_of_string matched_int)::(next_token input (pos + (String.length matched_int)))

    else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then
      let matched_str = Str.matched_string input in
      let clean = Str.string_after (Str.string_before matched_str (String.length matched_str - 1)) 1 in
      Tok_String (clean)::(next_token input (pos + (String.length matched_str)))
    
    else if Str.string_match (Str.regexp ")") input pos then 
      Tok_RParen::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp "(") input pos then
      Tok_LParen::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp "\\+") input pos then
      Tok_Add::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp "\\*") input pos then
      Tok_Mult::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp "/") input pos then
      Tok_Div::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp "-") input pos then
      Tok_Sub::(next_token input (pos + 1))

    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
      let matched_str = Str.matched_string input in
      Tok_ID (matched_str)::(next_token input (pos + (String.length matched_str)))
    
    else raise (InvalidInputException "lexer")

  in next_token input 0;;