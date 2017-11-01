(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* put your solutions for problem 1 here *)


(*1a*)
fun all_except_option (s, strs)=
  let fun get_list (z, s, strs)=
        case strs of
            [] => NONE
          | x::y => if same_string(s, x)
                    then SOME(z @ y)
                    else get_list(z @ [x], s, y)
  in get_list([], s, strs)
  end


(*1b*)
fun get_substitutions1 (strs_list, s)=
  case strs_list of
      [] => []
    | y::ys => case all_except_option(s, y) of
                   NONE => get_substitutions1(ys, s)
                 | SOME i => i @ get_substitutions1(ys, s)


(*1c*)
fun get_substitutions2 (strs_list, s)=
  let fun f (z, strs_list, s)=
        case strs_list of
            [] => z
          | y::ys => case all_except_option(s, y) of
                         NONE => f(z, ys, s)
                       | SOME i => f(z @ i, ys, s)
  in f([], strs_list, s)
  end


(*1d*)
fun similar_names (strs_list:string list list, full_name:{first:string, middle:string, last:string})=
  case full_name of
      {first=f, middle=m, last=l} => let val x = get_substitutions2(strs_list, f)
                                         fun f (z, x, m, l)=
                                           case x of
                                               [] => z
                                             | x::xs => f(z @ [{first=x, middle=m, last=l}], xs, m, l)
                                     in full_name::f([], x, m, l)
                                     end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
 

(*2a*)
fun card_color (c:card)=
  case c of
      (Clubs, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red
    | (Spades, _) => Black


(*2b*)
fun card_value (c:card)=
  case c of
      (_, Jack) => 10
    | (_, Queen) => 10
    | (_, King) => 10
    | (_, Ace) => 11
    | (_, Num i) => i


(*2c*)
fun remove_card (held:card list, c:card, e)=
  let fun return (x:card list, held:card list, c:card)=
        case held of
            [] => x
          | y::ys => if y = c
                     then x @ ys
                     else return(x @ [y], ys, c)
      val re = return([], held, c)
  in if re = held
     then raise e
     else re
  end


(*2d*)
fun all_same_color (held:card list)=
  case held of
      [] => true
    | x::xs => case xs of
                   [] => true
                 | y::ys => if card_color(x) = card_color(y)
                            then all_same_color(xs)
                            else false


(*2e*)
fun sum_cards (held:card list)=
  let fun sum (x:int, y:card list)=
        case y of
            [] => x
          | y1::y2 => sum(card_value(y1)+x, y2)
  in sum(0, held)
  end


(*2f*)
fun score (held:card list, goal:int)=
  let val s = sum_cards(held)
      fun cal (pre:int)= 
        if all_same_color(held)
        then pre div 2
        else pre
  in if s > goal
     then cal(3 * (s - goal))
     else cal(goal - s)
  end


(*2g*)
fun officiate (cs:card list, ms:move list, goal:int)=
  let fun moves (held:card list, cs:card list, ms:move list)=
        let val END = score(held, goal)
        in if sum_cards(held) > goal
           then END
           else case ms of
                    [] => END
                  | x::xs => case x of
                                 Discard c => moves(remove_card(held, c, IllegalMove), cs, xs)
                                          | Draw => case cs of
                                                        [] => END
                                                      | y::ys => moves(y::held, ys, xs)
        end
  in moves([], cs, ms)
  end




(*3a*)
fun score_challenge (held:card list, goal:int)=
  let fun count_ace (n:int, held:card list)=
        case held of
            [] => n
          | x::xs => case x of
                         (_, Ace) => count_ace(n + 1, xs)
                       | (_, _) => count_ace(n, xs)

      val N = count_ace(0, held)

      fun new_score_trans (n:int)=
        let val s = sum_cards(held) - n * 10
            fun cal (pre:int)=
              if all_same_color(held)
              then pre div 2
              else pre
        in if s > goal
           then cal(3 * (s - goal))
           else cal(goal - s)
        end

      fun best_score (z:int, n:int)=
        if n = 0
        then z
        else if new_score_trans(n) >= new_score_trans(n-1)
        then best_score(new_score_trans(n-1), n-1)
        else best_score(new_score_trans(n), n-1)

  in best_score(new_score_trans(0), N)
  end

fun officiate_challenge (cs:card list, ms:move list, goal:int)=
  let fun moves (held:card list, cs:card list, ms:move list)=
        let fun count_ace (n:int, held:card list)=
              case held of
                  [] => n
                | x::xs => case x of
                               (_, Ace) => count_ace(n + 1, xs)
                             | (_, _) => count_ace(n, xs)

            val s = sum_cards(held) - count_ace(0, held) * 10
            val END = score_challenge(held, goal)
        in if s > goal
           then END
           else case ms of
                    [] => END
                  | x::xs => case x of
                                 Discard c => moves(remove_card(held, c, IllegalMove), cs, xs)
                               | Draw => case cs of
                                             [] => END
                                           | y::ys => moves(y::held, ys, xs)
        end
  in moves([], cs, ms)
  end




(*3b*)
fun careful_player (cs:card list, goal:int)=
  let fun pick_card (held:card list, limit:int)=
        case held of
            [] => []
          | h::hs => if card_value(h) = goal - limit
                     then [Discard h]
                     else pick_card(hs, limit)

      fun choose_to_discard (held:card list, limit:int)=
        if limit < 1
        then []
        else case pick_card(held, limit) of
                 [] => choose_to_discard(held, limit-1)
               | c => c

      fun choice (held:card list)=
        if sum_cards(held) - goal >= 0
        then []
        else if goal - sum_cards(held) > 10
        then [Draw]
        else case held of
                 [] => [Draw]
               | x => case choose_to_discard(held, 10) of
                          [] => [Draw]
                        | y => y

      fun moves_list (held: card list, cs:card list, ms:move list)=
        case cs of
            [] => ms
          | x::xs =>case choice(held) of
                       [] => ms
                     | Draw::_ => moves_list(held@[x], xs, ms@[Draw])
                     | Discard c::_ => moves_list(remove_card(held, c, IllegalMove), cs, ms@[Discard c])
  in moves_list([], cs, [])
  end
      
