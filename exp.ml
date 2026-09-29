type exp =
| Var of string
| Int of int
| Add of exp * exp
| Mul of exp * exp
| Not of exp
| IfThenElse of exp * exp * exp

type environ = ( string * int ) list

let rec vars ( e : exp ) : string list =
match e with
| Var x -> [ x ]
| Int _ -> []
| Add (a , b ) | Mul (a , b ) -> ( vars a ) @ ( vars b )
| Not a -> vars a
| IfThenElse (a ,b , c ) -> ( vars a ) @ ( vars b ) @ ( vars c )

let rec eval ((env : environ) , (exp : exp)) : int = 
  match exp with
  | Var x -> List.assoc x env
  | Int n -> n
  | Add (a, b) -> eval (env, a) + eval (env, b)
  | Mul (a, b) -> eval (env, a) * eval (env, b)
  | Not a -> if eval (env, a) = 0 then 1 else 0
  | IfThenElse (a, b, c) ->
    begin
      if eval (env, a) <> 0 
      then eval (env, b) 
      else eval (env, c)
    end


let rec backport (e : exp) : exp = 
  match e with 
  | Var _ -> e
  | Int _ -> e
  | Add (a, b) -> Add ( backport a, backport b)
  | Mul (a, b) -> Mul ( backport a, backport b)
  | Not (a) -> Not (backport a)
  | IfThenElse (a, b, c) -> Add(Mul(Not(Not(backport a)), backport b), Mul(Not(backport a), backport c))
