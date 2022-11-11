(*Q1*)

datatype DBI = var of int | app of (DBI * DBI) | lambda of DBI


(*Q2*)

(*expression of lambda calculus*)

datatype expr = Var of Atom.atom
	      | Lambda of (Atom.atom * expr)
	      | App of expr * expr
(*diagA : string -> Atom.atom -> string *)
fun diagA x y =
		      let 
	  			val b = String.explode x
	   			val e = String.implode (b @ [#"a"])
	   			val t = String.implode (b @ [#"c"])
		      in
			  if (String.isPrefix e (Atom.toString(y))) then t
			  else e
		      end
;

(*helper : Atom.atom * string -> string*)

fun helper (x,y) = diagA y x


(* fresh : AtomSet -> string *)
fun fresh vars = if (AtomSet.isEmpty vars = true) then "b"
                    else AtomSet.foldl helper "" vars

(* converter : string list -> AtomRedBlackSet.set -> DBI -> expr *)
fun converter lst set (app (e1, e2)) = App (converter lst set e1, converter lst set e2)
    | converter lst set (lambda e) = let
                                        val freshvariable = fresh set
                                        val newlist = freshvariable :: lst
                                        val newst = AtomSet.add (set, Atom.atom freshvariable)
                                    in
                                        Lambda (Atom.atom freshvariable, converter newlist newst e)
                                    end
    | converter lst set (var e) = let
                                val eth = List.nth (lst, e - 1)
                            in
                                Var (Atom.atom eth)
                            end

(* convert : DBI -> expr *)
fun convert e = converter [] AtomSet.empty e

(* for printing expression of lambda calculus *)
(* exprprint : expr -> string *)
fun exprprint (Var x) = String.concat [Atom.toString x]
  | exprprint (App (e1, e2)) = String.concat ["(", exprprint e1, " ", exprprint e2, ")"]
  | exprprint (Lambda (x, e)) = String.concat ["(λ", Atom.toString x, ".", exprprint e, ")"]



