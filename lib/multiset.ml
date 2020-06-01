(* Copyright (c) 2013, Simon Cruanes All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. Redistributions in binary
   form must reproduce the above copyright notice, this list of conditions and
   the following disclaimer in the documentation and/or other materials provided
   with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE. *)

(** {1 Multiset} *)

type 'a sequence = ('a -> unit) -> unit

let max_int = max

let min_int = min

module type S = sig
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : t -> elt -> bool

  val count : t -> elt -> int

  val singleton : elt -> t

  val add : t -> elt -> t

  val remove : t -> elt -> t

  val add_mult : t -> elt -> int -> t
  (** [add_mult set x n] adds [n] occurrences of [x] to [set]

      @raise Invalid_argument if [n < 0]
      @since 0.6 *)

  val remove_mult : t -> elt -> int -> t
  (** [remove_mult set x n] removes at most [n] occurrences of [x] from [set]

      @raise Invalid_argument if [n < 0]
      @since 0.6 *)

  val remove_all : t -> elt -> t
  (** [remove_all set x] removes all occurrences of [x] from [set]

      @since 0.22 *)

  val update : t -> elt -> (int -> int) -> t
  (** [update set x f] calls [f n] where [n] is the current multiplicity of [x]
      in [set] ([0] to indicate its absence); the result of [f n] is the new
      multiplicity of [x].

      @raise Invalid_argument if [f n < 0]
      @since 0.6 *)

  val min : t -> elt
  (** Minimal element w.r.t the total ordering on elements *)

  val max : t -> elt
  (** Maximal element w.r.t the total ordering on elements *)

  val union : t -> t -> t
  (** [union a b] contains as many occurrences of an element [x] as
      [count a x + count b x]. *)

  val meet : t -> t -> t
  (** [meet a b] is a multiset such that
      [count (meet a b) x = max (count a x) (count b x)] *)

  val intersection : t -> t -> t
  (** [intersection a b] is a multiset such that
      [count (intersection a b) x = min (count a x) (count b x)] *)

  val diff : t -> t -> t
  (** MultiSet difference. [count (diff a b) x = max (count a x - count b x) 0] *)

  val contains : t -> t -> bool
  (** [contains a x = (count m x > 0)] *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val cardinal : t -> int
  (** Number of distinct elements *)

  val iter : t -> (int -> elt -> unit) -> unit

  val fold : t -> 'b -> ('b -> int -> elt -> 'b) -> 'b

  val of_list : elt list -> t

  val to_list : t -> elt list

  val to_seq : t -> elt sequence

  val of_seq : elt sequence -> t

  val of_list_mult : (elt * int) list -> t
  (** @since 0.19 *)

  val to_list_mult : t -> (elt * int) list
  (** @since 0.19 *)

  val to_seq_mult : t -> (elt * int) sequence
  (** @since 0.19 *)

  val of_seq_mult : (elt * int) sequence -> t
  (** @since 0.19 *)
end

module Make (O : Set.OrderedType) = struct
  module M = Map.Make (O)

  type t = int M.t

  type elt = O.t

  let empty = M.empty

  let is_empty = M.is_empty

  let mem ms x = M.mem x ms

  let count ms x = try M.find x ms with Not_found -> 0

  let singleton x = M.singleton x 1

  let add ms x =
    let n = count ms x in
    M.add x (n + 1) ms

  let add_mult ms x n =
    if n < 0 then invalid_arg "CCMultiSet.add_mult";
    if n = 0 then ms else M.add x (count ms x + n) ms

  let remove_mult ms x n =
    if n < 0 then invalid_arg "CCMultiSet.remove_mult";
    let cur_n = count ms x in
    let new_n = cur_n - n in
    if new_n <= 0 then M.remove x ms else M.add x new_n ms

  let remove ms x = remove_mult ms x 1

  let remove_all ms x = M.remove x ms

  let update ms x f =
    let n = count ms x in
    match f n with
    | 0 -> if n = 0 then ms else M.remove x ms
    | n' -> if n' < 0 then invalid_arg "CCMultiSet.update" else M.add x n' ms

  let min ms = fst (M.min_binding ms)

  let max ms = fst (M.max_binding ms)

  let union m1 m2 =
    M.merge
      (fun _x n1 n2 ->
        match (n1, n2) with
        | None, None -> assert false
        | Some n, None | None, Some n -> Some n
        | Some n1, Some n2 -> Some (n1 + n2))
      m1 m2

  let meet m1 m2 =
    M.merge
      (fun _ n1 n2 ->
        match (n1, n2) with
        | None, None -> assert false
        | Some n, None | None, Some n -> Some n
        | Some n1, Some n2 -> Some (max_int n1 n2))
      m1 m2

  let intersection m1 m2 =
    M.merge
      (fun _x n1 n2 ->
        match (n1, n2) with
        | None, None -> assert false
        | Some _, None | None, Some _ -> None
        | Some n1, Some n2 -> Some (min_int n1 n2))
      m1 m2

  let diff m1 m2 =
    M.merge
      (fun _x n1 n2 ->
        match (n1, n2) with
        | None, None -> assert false
        | Some n1, None -> Some n1
        | None, Some _n2 -> None
        | Some n1, Some n2 -> if n1 > n2 then Some (n1 - n2) else None)
      m1 m2

  let contains m1 m2 =
    try M.for_all (fun x c -> M.find x m1 >= c) m2 with Not_found -> false

  let compare m1 m2 = M.compare (fun x y -> x - y) m1 m2

  let equal m1 m2 = M.equal (fun x y -> x = y) m1 m2

  let cardinal m = M.cardinal m

  let iter m f = M.iter (fun x n -> f n x) m

  let fold m acc f = M.fold (fun x n acc -> f acc n x) m acc

  let of_list l =
    let rec build acc l =
      match l with [] -> acc | x :: l' -> build (add acc x) l'
    in
    build empty l

  let to_list m =
    (* [n_cons n x l] is the result of applying [fun l -> x :: l] [n] times to
       [l] *)
    let rec n_cons n x l =
      match n with 0 -> l | 1 -> x :: l | _ -> n_cons (n - 1) x (x :: l)
    in
    fold m [] (fun acc n x -> n_cons n x acc)

  let to_seq m k =
    M.iter
      (fun x n ->
        for _i = 1 to n do
          k x
        done)
      m

  let of_seq seq =
    let m = ref empty in
    seq (fun x -> m := add !m x);
    !m

  let of_list_mult l = List.fold_left (fun s (x, i) -> add_mult s x i) empty l

  let to_list_mult m = fold m [] (fun acc n x -> (x, n) :: acc)

  let to_seq_mult m k = M.iter (fun x n -> k (x, n)) m

  let of_seq_mult seq =
    let m = ref empty in
    seq (fun (x, n) -> m := add_mult !m x n);
    !m
end
