(*
Q1. Define abstract syntax for λ-let and λ-letrec as a SML datatype.
*)

(* λ-calculus SML datatype *)
datatype lambda = Variable of string
                | Apply of lambda * lambda
                | Lambda of string * lambda

(* λ-let SML datatype *)
datatype lambdalet = Variable_let of string
                   | Apply_let of lambdalet * lambdalet
                   | Lambda_let of string * lambdalet
                   | Let of string * lambdalet * lambdalet

(* λ-letrec SML datatype *)
datatype lambdaletrec = Variable_letrec of string
                      | Apply_letrec of lambdaletrec * lambdaletrec
                      | Lambda_letrec of string * lambdaletrec
                      | Letrec of string * lambdaletrec * lambdaletrec

(*
Q2. Write the conversion λ-let to λ-calculus.
*)

(* lambdacalc : lambdalet -> lambda *)
fun lambdacalc (Variable_let x)          = Variable x
  | lambdacalc (Apply_let (e1, e2)) = Apply (lambdacalc e1, lambdacalc e2)
  | lambdacalc (Lambda_let (x, e))  = Lambda (x, lambdacalc e)
  | lambdacalc (Let (x, e1, e2))    = Apply (Lambda (x, lambdacalc e2), lambdacalc e1)

