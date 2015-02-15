(** Abstract datatype and operations for (nested local) environments  *)

module PairSet = Set.Make(struct type t = int * (int list)
				 let compare = Pervasives.compare end)

type elem = PairSet.t

(*  eq : elem -> elem -> bool  *)
let eq = PairSet.equal

(*  leq : elem -> elem -> bool  *)
let leq env0 env1 = PairSet.subset env0 env1

(*  join : elem -> elem -> elem  *)
let join env0 env1 = PairSet.union env0 env1

(*  meet : elem -> elem -> elem  *)
let meet env0 env1 = PairSet.inter env0 env1

(*  bot : elem  *)
let bot = PairSet.empty


(** {3 Lattice operations } *)

(*  is_bot : EL -> bool  *)
let is_bot env = leq env bot

(*  init : label -> label -> elem *)
let init lenvlab genvlab = PairSet.add (lenvlab,[genvlab]) bot

(*  fold : ((int * int list) -> 'a -> 'a) -> elem -> 'a -> 'a *)
let fold = PairSet.fold

(*  enter_scope : EL -> label -> EL *)
let enter_scope env lab =
  fold (fun (envlab,scopechain) envpairs -> PairSet.add (lab,envlab::scopechain) envpairs)
    env PairSet.empty

(*  exit_scope : EL -> EL *)
let exit_scope env =
  fold (fun (envlab,scopechain) envpairs -> match scopechain with
         | [] -> failwith "Leaving global environment\n";
	 | outerlab::scopechain' -> PairSet.add (outerlab,scopechain') envpairs)
    env PairSet.empty


(** {3 Pretty printing routines } *)

(*  pprint : EL -> unit *)
let pprint elat =
  (*  pprint_list : int list -> unit *)
  let pprint_list is =
    begin
      Format.printf "[@[<h 1>";
      ignore (List.fold_left (fun first i ->
	  if first
	  then (Format.print_int i; false)
	  else
	    begin
	      Format.printf ",";
	      Format.print_space ();
	      Format.print_int i;
	      false
	    end) true is);
      Format.printf "@]]";
    end in
  (*  pprint_pair : int * int list -> unit *)
  let pprint_pair (i,is) =
    begin
      Format.printf "(%i," i;
      pprint_list is;
      Format.printf ")"
    end in
  begin
    Format.printf "{ @[<h 1>";
    ignore (fold (fun p first -> 
      if first
      then (pprint_pair p; false)
      else
	begin
	  Format.printf ",";
	  Format.print_space ();
	  pprint_pair p;
	  false
	end) elat true);
    Format.printf "@] }";
  end

(*  to_string : elem -> string  *)
let to_string el =
  let buf = Buffer.create 64 in
  let out,flush = Format.get_formatter_output_functions () in (* save previous outputters *)
  begin
    Format.set_formatter_output_functions (Buffer.add_substring buf) (fun () -> ());
    pprint el;
    Format.print_flush ();
    Format.set_formatter_output_functions out flush;          (* restore previous outputters *)
    Buffer.contents buf
  end
