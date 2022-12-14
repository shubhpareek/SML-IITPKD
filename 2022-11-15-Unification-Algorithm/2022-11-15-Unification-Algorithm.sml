 signature SIGNATURE = sig

    type symbol   (* This type captures the symbols of the signature *)
    val arity   : symbol -> int
    val compare : symbol * symbol -> order  (* So that you can use it during unification *)

  end (* module type SIGNATURE *)


functor Unify (S : SIGNATURE) = struct
  datatype term = VARIABLE of Atom.atom
  | APPLY of S.symbol * term list

  type telescope = term AtomMap.map
  type equation = term * term
  
  (*val checkforrecursion :  term AtomRedBlackMap.map option -> Atom.atom -> term -> bool*)
  fun checkforrecursion tel y (VARIABLE(x)) = 
      if Atom.same(x,y)
      then false
      else
        (
          if (Option.isSome (AtomMap.find (Option.valOf (tel),x))) then
             checkforrecursion tel y (Option.valOf (AtomMap.find (Option.valOf (tel),x))) 
          else	  false
        )
        
    | checkforrecursion tel y (APPLY(s,l)) =  not (List.null (List.filter (checkforrecursion tel y) l ) ) 

   (*val helper : 'a list -> 'b list -> ('a * 'b) list*)
   fun helper [] [] = []
    |  helper (x::xs) (y::ys) = ((x,y)::(helper xs ys))

  (* val unifyVar : term AtomRedBlackMap.map option -> Atom.atom -> term -> telescope option*)
  fun unifyVar tel va ter = case (Option.isSome (AtomMap.find (Option.valOf (tel) ,va))) of
        true =>  unify (((Option.valOf (AtomMap.find (Option.valOf (tel),va))),ter),tel)  
      | false => 
        case (checkforrecursion tel va ter) of
          true => Option.NONE
        | false => Option.SOME(AtomMap.insert(Option.valOf (tel),va,ter))
  and
    unify ((sEQt : equation),(tel : telescope option)) : telescope option =
      if not(Option.isSome tel)
      then
        	Option.NONE
      else
		case sEQt of 
		  (VARIABLE x, t )                            => unifyVar tel x t
		  
		|  (s    , VARIABLE y)                         => unifyVar tel y s
		
		|  (APPLY (f, argsoff), APPLY (g, argsofg) )  => if (S.compare (f,g)) = EQUAL
		  then
		    List.foldl unify tel (helper argsoff argsofg)
		                                                
		  else
		    Option.NONE
	    
                                                        
end
