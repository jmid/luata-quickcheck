(** Abstract string datatype and operations *)

type elem =
  | Bot
  | Const of string
  | Top

(*  eq : elem -> elem -> bool  *)
let eq (a:elem) a' = a = a'

(*  leq : elem -> elem -> bool  *)
let leq s s' = match s,s' with
  | Bot,_ -> true
  | _,Bot -> false
  | _,Top -> true
  | Top,_ -> false
  | Const c, Const c' -> c=c'

(*  join : elem -> elem -> elem  *)
let join s s' = match s,s' with
  | Bot,_ -> s'
  | _,Bot -> s
  | Top,_ -> Top
  | _,Top -> Top
  | Const c, Const c' -> if c=c' then s else Top

(*  meet : elem -> elem -> elem  *)
let meet s s' = match s,s' with
  | Bot,_ -> Bot
  | _,Bot -> Bot
  | Top,_ -> s'
  | _,Top -> s
  | Const c, Const c' -> if c=c' then s else Bot

(* const : string -> elem  *)
let const s = Const s

(*  bot : elem  *)
let bot = Bot

(*  top : elem  *)
let top = Top

(*  upper : elem -> elem *)
let upper s = match s with
  | Bot -> Bot
  | Top -> Top
  | Const c -> Const (String.uppercase c)

(*  lower : elem -> elem *)
let lower s = match s with
  | Bot -> Bot
  | Top -> Top
  | Const c -> Const (String.lowercase c)

(*  concat : elem -> elem -> elem *)
let concat s s' = match s,s' with
  | Bot,_ -> Bot
  | _,Bot -> Bot
  | Top,_ -> Top
  | _,Top -> Top
  | Const c, Const c' -> Const (c ^ c')

(*  empty : elem -> bool  *)
let empty s = s = Bot

(*  nonempty : elem -> bool  *)
let nonempty s = s <> Bot


(** {3 Pretty printing routine } *)

(*  to_string : elem -> unit  *)
let to_string s = match s with
  | Bot     -> "Bot"
  | Const c -> "Const \"" ^ c ^ "\""
  | Top     -> "Top"

(*  pprint : elem -> unit  *)
let pprint s = match s with
  | Bot     -> Format.printf "Bot"
  | Const c -> Format.printf "Const \"%s\"" c
  | Top     -> Format.printf "Top"
