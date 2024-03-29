
(*Q1*)

fun curry    f  a b c  = f (a,b,c)
fun uncurry  f (a,b,c) = f a b c

(*Q2*)

(*
type of function fn and snd

val fst = fn : 'a * 'b -> 'a
val snd = fn : 'a * 'b -> 'b
*)

fun fst (a,b) = a
fun snd (a,b) = b


(*Q3*)

(*type len [1,2,3,4] and it will return 4 *)

fun len [] = 0 |
    len (x::xs) = 1+ len xs

(*Q4*)

(* to reverse just call for example reverse [1,2,3,4] *)

fun stack xs [] = xs 
  | stack xs (y::ys) = stack (y::xs) ys

fun reverse [] = [] | reverse xs = stack [] xs  (* O(n) *)
    

(*Q5*)

(* FIBONNACI * F(N) = F(N-1) + F(N-2) 0,1,1,2,3,5*)

fun forfibo n x pp p =  if x=n then (pp + p)
  			 else forfibo n (x+1) p (pp + p)

fun fibo 0 = 0 | 
    fibo 1 = 1 |
    fibo n = forfibo n 2 0 1
    
(*for calculating type fibo n where n is the position of the number in series *)





