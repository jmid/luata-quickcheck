(** Abstract absence datatype and operations *)

type elem =
  | Must
  | May

(*  bot : elem *)
let bot = Must
let is_present = bot

(*  top : elem *)
let top = May
let maybe_absent = top

(*  eq : elem -> elem -> bool  *)
let eq (a:elem) a' = a = a'

(*  leq : elem -> elem -> bool *)
let leq a a' = match a with
  | Must -> true
  | May  -> (a' = May)

(*  join : elem -> elem -> elem *)
let join a a' = match a with
  | Must -> a'
  | May  -> May

(*  meet : elem -> elem -> elem *)
let meet a a' = match a with
  | Must -> Must
  | May  -> a'


(** {3 Pretty printing routine } *)

(*  to_string : elem -> unit  *)
let to_string a = match a with
  | Must -> "!"
  | May  -> "?"

(*  pprint : elem -> unit  *)
let pprint a = match a with
  | Must -> Format.printf "!"
  | May  -> Format.printf "?"
