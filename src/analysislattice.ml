(** Overall analysis lattice *)

module LabelMap = Map.Make(struct type t = int
				  let compare i i' = i - i' end)

type elem = Statelattice.elem LabelMap.t

(*  leq : elem -> elem -> bool  *)
let leq st0 st1 =
  LabelMap.fold (fun lab obj acc ->
    let obj' = try LabelMap.find lab st1
               with Not_found -> Statelattice.bot
    in acc && Statelattice.leq obj obj') st0 true

(*  eq : elem -> elem -> bool  *)
let eq = LabelMap.equal (Statelattice.eq)

(*  join : elem -> elem -> elem  *)
let join st0 st1 = LabelMap.merge (fun lab obj0 obj1 -> match obj0,obj1 with
  | None,_ -> obj1
  | _,None -> obj0
  | Some obj0,Some obj1 -> Some (Statelattice.join obj0 obj1)) st0 st1

(*  meet : elem -> elem -> elem  *)
let meet st0 st1 = LabelMap.merge (fun lab obj0 obj1 -> match obj0,obj1 with
  | None,_ -> None
  | _,None -> None
  | Some obj0,Some obj1 -> Some (Statelattice.meet obj0 obj1)) st0 st1

(*  bot : elem  *)
let bot = LabelMap.empty


(** {3 Lattice operations } *)

(*  init : elem  *)
let init = LabelMap.empty

(*  lookup : elem -> label -> Statelattice.elem  *)
let lookup lat lab =
  try LabelMap.find lab lat
  with Not_found -> Statelattice.bot

(*  add : elem -> label -> Statelattice.elem -> elem  *)
let add lat lab sl =
  LabelMap.add lab sl lat


(** {2 Pretty printing} *)

(*  pprint : elem -> unit  *)
let pprint alat =
  begin
    Format.printf "{ @[<v 0>";
    ignore (LabelMap.fold (fun lab slat first -> 
                             begin
			       (if first then () else Format.print_space ());
			       Format.printf "%-is -> " lab;
			       Statelattice.pprint slat;
			       false
			     end) alat true);
    Format.printf "@] }";
  end

(*  to_string : elem -> string  *)
let to_string alat =
  let buf = Buffer.create 512 in
  let out,flush = Format.get_formatter_output_functions () in (* save previous outputters *)
  begin
    Format.set_formatter_output_functions (Buffer.add_substring buf) (fun () -> ());
    pprint alat;
    Format.print_flush ();
    Format.set_formatter_output_functions out flush;          (* restore previous outputters *)
    Buffer.contents buf
  end
