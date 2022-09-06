(*Q1*)
(*val map = fn : ('a -> 'b) -> 'a list -> 'b list*)

fun map f []         = []
  | map f (x :: xs)  = f x :: map f xs

(*Q2*)
(* datatype 'a tree = node of 'a tree * 'a * 'a tree | nulltree *)
datatype 'a tree = nulltree | node of 'a tree * 'a * 'a tree


(*Q3*)
(* val maptree = fn : ('a -> 'b) -> 'a tree -> 'b tree *)

fun maptree f nulltree = nulltree 
  | maptree f (node(tl,x,tr)) = node( maptree f tl ,f x, maptree f tr )
    
(*Q4*)
(*
val inorder = fn : 'a tree -> 'a list
val preorder = fn : 'a tree -> 'a list
val postorder = fn : 'a tree -> 'a list
*)

fun inorder nulltree = [] |
    inorder (node(tl,x,tr))= ( inorder tl ) @ x::( inorder tr )
    
fun preorder nulltree = []
    | preorder (node(tl,x,tr))= x::(( preorder tl ) @ ( preorder tr ))
    
fun postorder nulltree = []
  | postorder (node(tl,x,tr))= (( postorder tl ) @ ( postorder tr ))@(x::nil)



(*Q5*)
(* val rot_c = fn : 'a tree -> 'a tree *)


fun rot_c   (node(node(tl, b, t2), a, t3)) = node(tl, b,node(t2 , a, t3)) 
  | rot_c x = x 

(*
below there are commands for tree construction , the root of this tree is lf2 to use this tree use commands like 
-> rot_c lf2
-> inorder lf2
maptree can also be used on this 
the tree looks like
				 	   2
					 /   \
					3     1
				       / \
				      5   4
*)				      

(*		TREE CONSTRUCTION COMMANDS
					
val lf4 = node(nulltree , 4 , nulltree );
val lf5 = node(nulltree , 5 , nulltree );
val lf3 = node(lf5 , 3 , lf4 );
val lf1 = node(nulltree,1,nulltree);
val lf2 = node(lf3,2,lf1);

- inorder lf2 ;
val it = [5,3,4,2,1] : int list

*)

