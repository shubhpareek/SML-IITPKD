(*Q1*)

type var = Atom.atom
datatype expr =  Var of var
		| Apply of expr * expr 
		| Lambda of var * expr
;
(*Q2*)
fun  free (Var x)        = AtomSet.add(AtomSet.empty,x)
   | free (Apply (x,y))  = AtomSet.union( (free x) , (free y))
   | free (Lambda (x,y)) = AtomSet.subtract( (free y ), x)
;
fun  bound (Var x)        = AtomSet.empty
   | bound (Apply (x,y))  = AtomSet.union( (bound x) , (bound y))
   | bound (Lambda (x,y)) = AtomSet.add( (bound y), x)
;
(*Q3*)

fun subst (Var z) y x 	      = if Atom.same(y,z) then x else (Var z)
   |subst (Apply (a,b)) y x  = Apply((subst a y x),(subst b y x))
   |subst (Lambda (v,e)) y x = if Atom.same(v,y) then 
   					Lambda (y,e)
   				else
   					Lambda (y,subst e y x)
;
(*Q4*)
(*using diagonalising argument*)
fun diagA (y,x) =
		      let 
	  			val b = String.explode x
	   			val e = String.implode (b @ [#"a"])
	   			val t = String.implode (b @ [#"c"])
		      in
			  if (String.isPrefix e (Atom.toString(y))) then t
			  else e
		      end
;

fun fresh a = Atom.atom(AtomSet.foldl diagA "" a)
;

val x = AtomSet.fromList([Atom.atom("a"),Atom.atom("aa"),Atom.atom("ca")]);
Atom.toString(fresh x);	

