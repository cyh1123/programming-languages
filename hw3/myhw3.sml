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
(*1*)
fun only_capitals x = List.filter (fn y => Char.isUpper(String.sub(y, 0))) x

(*2*)
fun longest_string1 x = foldl (fn (y, z) => if String.size y > String.size z then y else z) "" x

(*3*)
fun longest_string2 x = foldl (fn (y, z) => if String.size y >= String.size z then y else z) "" x

(*4*)
fun longest_string_helper f = foldl (fn (y, z) => if f (String.size y, String.size z) then y else z) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(*5*)
val longest_capitalized = longest_string1 o only_capitals

(*6*)
val rev_string = implode o rev o explode

(*7*)
fun first_answer f x =
  case List.filter (fn y => case y of SOME _ => true | _ => false) (map f x) of
      SOME i :: _ => i
    | _ => raise NoAnswer

(*8*)
fun all_answers f x =
  let val s = foldl (fn (a, b) => case f a of SOME c => b@c | _ => b ) [] x
  in case x of
         [] => SOME []
       | _ => if length s < length x
              then NONE
              else SOME s
  end


(*9a*) 
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(*9b*)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size

(*9c*) 
fun count_some_var (s, p) =
  let val f2 = fn x => if s = x then 1 else 0
  in g (fn _ => 0) f2 p
  end

(*10*)
fun check_pat p =
  let fun get_var p l =
        case p of
            Variable s => s::l
          | TupleP ps => foldl (fn (x, y) => get_var x y) l ps
          | _ => l
      val lis = get_var p []
      fun unique (strs:string list) =
        case strs of
            [] => false
          | _::[] => true
          | x::xs =>if List.exists (fn y => x = y) xs
                    then false
                    else unique xs
  in unique lis
  end

(*11*)
fun match (x, y) =
  case (x, y) of
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const v, ConstP p) => SOME []
    | (Tuple vs, TupleP ps) => if length vs = length ps
                               then all_answers match (ListPair.zip(vs, ps))
                               else NONE
    | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2
                                                   then match(v, p)
                                                   else NONE
    | (_, _) => NONE

(*12*)
fun first_match v ps = SOME (first_answer (fn x => match(v, x)) ps)
                       handle NoAnswer => NONE


(*challenge problem*)

  
