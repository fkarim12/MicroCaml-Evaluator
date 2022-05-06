open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

let match_op op x y =
  match op with
  |Add -> Int (x + y)
  |Sub -> Int (x - y)
  |Mult -> Int (x * y)
  |Div -> if y = 0 then raise (DivByZeroError) else (Int (x / y))
  |Greater -> if x > y then (Bool true) else (Bool false)
  |Less -> if x < y then (Bool true) else (Bool false)
  |GreaterEqual -> if x >= y then (Bool true) else (Bool false)
  |LessEqual -> if x <= y then (Bool true) else (Bool false)
  |Equal -> if x = y then (Bool true) else (Bool false)
  |NotEqual -> if x = y then (Bool false) else (Bool true)
  |_ -> raise (TypeError "binop")

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  |Value v -> v
  |ID v -> lookup env v
  |Not expr -> (let expr2 = eval_expr env expr in
    match expr2 with
    |Bool x -> Bool (not x)
    |_ -> raise (TypeError "Expected type bool")
  )
  |Binop (op, expr1, expr2) ->
    (let a = eval_expr env expr1 in
    let b = eval_expr env expr2 in
    match a, b with
    |Int v1, Int v2 -> (let x = v1 in let y = v2 in (match_op op x y))
    |Bool v1, Bool v2 -> (let x = v1 in let y = v2 in 
      match op with
      |Or -> if (x = true || y = true) then (Bool true) else (Bool false)
      |And -> if (x = true && y = true) then (Bool true) else (Bool false)
    )
    |String v1, String v2 -> (let x = v1 in let y = v2 in (String (x ^ y)))
    |_, _ -> raise (TypeError "")
  )
  |If (expr1, expr2, expr3) -> (let guard = eval_expr env expr1 in
    match guard with
    |Bool true -> eval_expr env expr2
    |Bool false -> eval_expr env expr3
    |_ -> raise (TypeError "Expected type bool")
  )
  |Let (x, r, expr1, expr2) ->
    (if r = false then 
      let v = eval_expr env expr1 in
      eval_expr (extend env x v) expr2
    else
      let env2 = extend_tmp env x in
      let temp = eval_expr env2 expr1 in
      update env x temp;
      eval_expr env2 expr2
  )
  |Fun (var, expr) -> Closure (env, var, expr)
  |FunctionCall (expr1, expr2) -> (let close = eval_expr env expr1 in
      match close with
      |Closure (env2, var, expr) -> (let v = eval_expr env expr2 in
        eval_expr (extend env2 var v) expr
      )
      |_ -> raise (TypeError "expected type closure")
  )

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  |Expr (expr) -> (env, Some (eval_expr env expr))
  |Def (var, expr) -> (let v = eval_expr env expr in
    let e1 = extend env var v in
    (e1, Some v)
  )
  |NoOp -> (env, None)