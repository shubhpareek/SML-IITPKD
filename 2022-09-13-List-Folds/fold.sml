(*Q1.*)
(*val foldr = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)

fun foldr f x [] = x |
foldr f x (xr::xs) = f( xr , foldr f x xs )

(*val foldl = fn : ('a * 'b -> 'a) -> 'a -> 'b list -> 'a *)

fun foldl f x [] = x 
  | foldl f x (xr::xs) = foldl  f (f( x , xr ))  xs 

(*Q2.*)

(*val add = fn : int * int -> int *)
fun add (x,y)  = x + y

(*val sum = fn : int list -> int *)

fun sum z = foldl add 0 z 

(*Q3.*)

(*val partition = fn : ('a -> bool) -> 'a list -> 'a list * 'a list *)

fun partition pred xs = let
                            fun help (x, (us, vs)) = if pred x then
                                                         (x :: us, vs)
                                                     else
                                                         (us, x :: vs)
                        in
                            foldr help ([], []) xs
                        end

(*val map = fn : ('a -> 'b) -> 'a list -> 'b list *)
                        
fun map f xs = let
                   fun help (x, y) = f x :: y
               in
                   foldr help [] xs
               end


(*val rhelp = fn : 'a list * 'a -> 'a list *)

fun rhelp (x,xs) = (xs::x)

(*val reverse = fn : 'a list -> 'a list *)

fun reverse x = foldl rhelp [] x

(**)

datatype 'a option = SOME of 'a | NONE;

datatype 'a Find = LookingFor of int
                 | Found      of 'a


(* val nthAux = fn : 'a list * int -> 'a Find *)

fun nthAux (x, y) = let
                        fun help (Found v, u)      = Found v
                          | help (LookingFor v, u) = if v = 0 then
                                                         Found u
                                                     else
                                                         LookingFor (v - 1)
                    in
                        foldl help (LookingFor y) x
                    end;

(* val nth = fn : 'a list * int -> 'a option *)

fun nth (x, y) = case (nthAux (x, y)) of
                     LookingFor _ => NONE
                   | Found v      => SOME v
                   
                   
                   
                   

