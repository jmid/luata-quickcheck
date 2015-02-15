(** Abstract number datatype and operations *)

type elem =
  | Bot
  | Top

(*  bot : elem *)
let bot = Bot

(*  top : elem *)
let top = Top

(*  eq : elem -> elem -> bool  *)
let eq (a:elem) a' = a = a'

(*  leq : elem -> elem -> bool *)
let leq a a' = match a with
  | Bot -> true
  | Top -> (a' = Top)

(*  join : elem -> elem -> elem *)
let join a a' = match a with
  | Bot -> a'
  | Top -> Top

(*  meet : elem -> elem -> elem *)
let meet a a' = match a with
  | Bot -> Bot
  | Top -> a'


(** {3 Pretty printing routine } *)

(*  to_string : elem -> unit  *)
let to_string a = match a with
  | Bot -> "Bot"
  | Top -> "Top"

(*  pprint : elem -> unit  *)
let pprint a = match a with
  | Bot -> Format.printf "Bot"
  | Top -> Format.printf "Top"
