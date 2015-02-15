(** Basic AST for Lua *)

type unop =
| Not
| Length
| Uminus

type binop =
| Eq | Lt | Gt | Ne | Le | Ge
| Plus | Minus | Times | Div | Mod | Pow
| Concat

type str =
| Normal of string
| Char of string
| Long of string

type number =
| Int of int
| Hex of int
| Float of float

type lit =
| Nil
| Bool of bool
| String of str
| Number of number
| Table of exp list * (string * exp) list
| Fun of string list * block (*fixme: pos*)

and lvalue =
| Name of string
| Index of exp * string
| DynIndex of exp * exp

and exp =
| Lit of lit
| Lvalue of lvalue
| Unop of unop * exp
| Binop of exp * binop * exp
| And of exp * exp
| Or of exp * exp
| Call of exp * exp list
| Methcall of exp * string * exp list
| Paren of exp

and stmt = { stmt_pos : Lexing.position;
	     stmt     : stmt_desc }
and stmt_desc =
| Break
| If of exp * block * block
| WhileDo of exp * block
(*| For of string * exp * exp * block*)
| Doend of block 
| Assign of lvalue list * exp list
| Local of string list * exp list
| Callstmt of exp * exp list
| Methcallstmt of exp * string * exp list
| Return of exp list

and block = stmt list
