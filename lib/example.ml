(* Variable declaration *)

let a = 5

let name = "OCaml"

(* Records *)
type language = { name : string; created : int }

(* Function declaration *)
let hello name = "Hello " ^ name

let () = print_endline (hello name)

(* Module signature *)
module type PLUS = sig
  type t

  (* Infix function *)
  val ( + ) : t -> t -> t
end

(* Module structure definition *)
module Plus_int = struct
  type t = int

  let ( + ) = Int.add
end

(* Module functor *)
module Sum (P : PLUS) = struct
  include P

  let list = List.fold_left ( + )
end

(* New module from functor *)
module Sum_int = Sum (Plus_int)

(* New module from functor using anonymous module*)
module Sum_float = Sum (struct
  type t = float

  let ( + ) = Float.add
end)

(* Using code from module *)
let () =
  print_float (Sum_float.list 0. [ 10.; 11.; 12. ]);
  print_int (Sum_int.list 0 [ 10; 11; 12 ])
