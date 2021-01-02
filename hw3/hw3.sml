(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals strings = 
    List.filter (fn s => Char.isUpper(String.sub(s,0))) strings

fun longest_string1 strings =
    List.foldl (fn (curr, acc) => let
		    val sizeCurr = String.size(curr)
		    val sizeAcc = String.size(acc)
		in
		    if (sizeCurr > sizeAcc)
		    then curr
		    else acc
		end) "" strings

fun longest_string2 strings =
    List.foldl (fn (curr, acc) => let
                    val sizeCurr = String.size(curr)
                    val sizeAcc = String.size(acc)
                in
                    if (sizeCurr >= sizeAcc)
		    then curr
                    else acc
                end) "" strings

fun longest_string_helper f strings =
     List.foldl (fn (curr, acc) => let
                    val sizeCurr = String.size(curr)
                    val sizeAcc = String.size(acc)
                 in
		     if f(sizeCurr, sizeAcc)
		     then curr
		     else acc
		 end) "" strings

val longest_string3 =
    longest_string_helper (fn (x,y) => x > y)

val longest_string4 =
    longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized =
    longest_string3 o only_capitals

val rev_string =
    String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME v => v
		    | NONE => first_answer f xs'

fun all_answers f xs =
    let
	fun aux (xs, acc) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      SOME v => aux(xs', acc @ v)
			    | NONE => NONE
    in
	aux(xs, [])
    end

val count_wildcards =
    g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths =
    g (fn () => 1) (fn s => String.size(s))

fun count_some_var (str,pat) =
    g (fn () => 0) (fn s => if (s = str) then 1 else 0) pat

fun check_pat p =
    let
	fun all_strs p =
	    case p of
		Wildcard          => []
              | Variable x        => [x]
              | TupleP ps         => List.foldl (fn (p,i) => i @ (all_strs p)) [] ps
              | ConstructorP(_,p) => all_strs p
              | _                 => []
	fun has_repeats xs =
	    case xs of
		[] => false
	      | x::xs' => if List.exists (fn s => s = x) xs'
			  then true
			  else has_repeats(xs')
    in
	not (has_repeats(all_strs p))
    end

fun match(v, p) =
    case (v,p) of
	(_, Wildcard) => SOME []
      | (v, Variable s)  => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i = j
			       then SOME []
			       else NONE
      | (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps)
				 then all_answers (fn (v,p) => match(v,p)) (ListPair.zip(vs,ps))
				 else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1 = s2
						  then match(v,p)
						  else NONE
      | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE

fun typecheck_patterns (ts, ps) =
    let		 
        (* Redefining this here to work for non-list values *)
	fun all_answers f xs =
	    let
		fun aux (xs, acc) =
		    case xs of
			[] => SOME acc
		      | x::xs' => case f x of
				      SOME v => aux(xs', acc @ [v])
				    | NONE => NONE
	    in
		aux(xs, [])
	    end

	fun pattern_to_type p =
	    let
		fun datatype_match(s,p) =
		    case (List.find (fn (con_name, _, t) => con_name = s andalso t = pattern_to_type(p)) ts) of
			SOME(_, dt_name, _) => Datatype dt_name
		      | _ => raise NoAnswer
	    in
		case p of
		    UnitP => UnitT
		  | ConstP _ => IntT
		  | TupleP ps => TupleT (List.map pattern_to_type ps)
		  | ConstructorP(s,con_p) => datatype_match(s,con_p) 
		  | _ => Anything  
	    end

	fun unify (t1,t2) =
	    case (t1, t2) of
		(t, Anything) => SOME t
	      | (Anything, t) => SOME t
	      | (TupleT ts1, TupleT ts2) => if List.length(ts1) = List.length(ts2)
					    then case (all_answers (fn (t1,t2) => unify(t1,t2)) (ListPair.zip(ts1,ts2))) of
						     SOME tl => SOME (TupleT tl)
						   | _ => NONE
					    else NONE
	      | (Datatype dt1, Datatype dt2) => if dt1 = dt2
						then SOME (Datatype dt1)
						else NONE
	      | (UnitT, UnitT) => SOME UnitT
	      | (IntT, IntT) => SOME IntT
	      | (_,_) => NONE

	fun fold_types ts =
	    case ts of
		[t] => SOME t
	      | t::t'::ts' => case unify(t, t') of
				  SOME unified_type => fold_types (unified_type::ts')
				| NONE => NONE
    in
	fold_types (List.map (pattern_to_type) ps)
	handle NoAnswer => NONE
    end
	


		      

