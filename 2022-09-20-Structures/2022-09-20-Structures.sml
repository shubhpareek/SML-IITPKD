signature SORT = sig
 	type t
 	val sort : t list -> t list
 end ;

 signature ORD_KEY =
 sig
 	type ord_key
     (* abstract type of keys that have a total order *)

 	val compare : ord_key * ord_key -> order
     (* comparison function for the ordering on keys *)

 end (* ORD_KEY *) ;
 
 (*Q1*)
    
 functor QSort ( O : ORD_KEY ) : SORT = struct
 	type t = O.ord_key 
 	fun sort [] = [] 
 	| sort (x::xs) = 
 	let 
 		val (left,right) = List.partition (fn y => (O.compare (y,x) = LESS) ) xs (*PARTITION BY PIVOT*)
 	in 
 		sort left @ [x] @ sort right	(*THEN JOIN THE TWO PARTS*)
 	end

 end;
 
 (*Q2*)
 
 structure IntOrd : ORD_KEY = struct 
 	type ord_key = int 
 	val compare = Int.compare
 	end;
 	
 structure sorter = QSort(IntOrd);
 
 sorter.sort [3,4,2,1,5] (*WORKING EXAMPLE*)
;

