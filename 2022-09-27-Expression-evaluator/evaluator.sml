
(*
Q1. 
*)
datatype Expr = Const of real
			  | Var   of string
			  | Plus  of Expr * Expr
			  | Mul   of Expr * Expr

datatype Stmt = Assign of string * Expr
			  | Print  of Expr

type Program = Stmt list


(*
Q2. 
*)

type Env = real AtomMap.map

(* Expression evaluator *)
(* eval :  Env -> Expr -> real option *)
fun eval  _  (Const x)        = SOME x
  | eval  env (Var x)         = AtomMap.find (env, Atom.atom x)
  | eval  env (Plus (x, y))   =
		let
			val res = case (eval env x ) of
						NONE        => NONE
					  | (SOME val1) => case (eval env y ) of
					  					NONE        => NONE
									  | (SOME val2) => SOME (val1 + val2)
		in
			res
		end
  | eval  env (Mul (x, y))    =
		let
			val res = case (eval env x ) of
						NONE        => NONE
					  | (SOME val1) => case (eval env y ) of
					  					NONE        => NONE
									  | (SOME val2) => SOME (val1 * val2)
		in
			res
		end

(* execute : Env -> Stmt -> Env *)
fun execute  env (Assign (x, y)) =
		let
		  val res = case (eval env y) of
						NONE        => env
					  | (SOME val2) => AtomMap.insert (env, Atom.atom x, val2)
		in
		  res
		end
  | execute env       (Print x) = 
		let
		  val res = case (eval env x) of
						NONE		=> ((print "NONE\n"); env)
					  | (SOME val1) => ((print (Real.toString val1)); (print "\n"); env)
		in
		  res
		end

(*
Q3. 
*)

fun temp (a,b) = execute b a
	
fun interpret [] = ()
	| interpret myprogram = (List.foldl temp AtomMap.empty myprogram;())
;

(*Example that you can use *)
(*val a = interpret [Assign("d",Const 3.0 ),Print ( Var "d" ),Assign("d",Plus( Var "d" , Const 5.0 )),Print (Var "d")];*)


