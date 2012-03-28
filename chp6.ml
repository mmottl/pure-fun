(*
   Original source code in SML from:

     Purely Functional Data Structures
     Chris Okasaki
     Cambridge University Press, 1998
     Copyright (c) 1998 Cambridge University Press

   Translation from SML to OCAML (this file):

     Copyright (C) 1999, 2000, 2001  Markus Mottl
     email:  markus.mottl@gmail.com
     www:    http://www.ocaml.info

   Licensed under the Apache License, Version 2.0 (the "License"); you may
   not use this file except in compliance with the License.  You may obtain
   a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
   License for the specific language governing permissions and limitations
   under the License.
*)

(***********************************************************************)
(*                              Chapter 6                              *)
(***********************************************************************)

exception Empty
exception Impossible_pattern of string

let impossible_pat x = raise (Impossible_pattern x)


module type QUEUE = sig
  type 'a queue

  val empty : 'a queue
  val is_empty : 'a queue -> bool

  val snoc : 'a queue -> 'a -> 'a queue
  val head : 'a queue -> 'a        (* raises Empty if queue is empty *)
  val tail : 'a queue -> 'a queue  (* raises Empty if queue is empty *)
end


(* A totally ordered type and its comparison functions *)
module type ORDERED = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end


module type HEAP = sig
  module Elem : ORDERED

  type heap

  val empty : heap
  val is_empty : heap -> bool

  val insert : Elem.t -> heap -> heap
  val merge : heap -> heap -> heap

  val find_min : heap -> Elem.t  (* raises Empty if heap is empty *)
  val delete_min : heap -> heap  (* raises Empty if heap is empty *)
end


(* ---------- Streams as found in chapter 4 ---------- *)

let (!$) = Lazy.force

module type STREAM = sig
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t

  val (++) : 'a stream -> 'a stream -> 'a stream  (* stream append *)
  val take : int -> 'a stream -> 'a stream
  val drop : int -> 'a stream -> 'a stream
  val reverse : 'a stream -> 'a stream
end

module Stream : STREAM = struct
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t

  let rec (++) s1 s2 =
    lazy (
      match s1 with
      | lazy Nil -> Lazy.force s2
      | lazy (Cons (hd, tl)) -> Cons (hd, tl ++ s2))

  let rec take n s =
    lazy (
      if n = 0 then Nil
      else
        match s with
        | lazy Nil -> Nil
        | lazy (Cons (hd, tl)) -> Cons (hd, take (n - 1) tl))

  let rec drop n s =
    lazy (
      match n, s with
      | 0, _ -> !$s
      | _, lazy Nil -> Nil
      | _, lazy (Cons (_, tl)) -> !$ (drop (n - 1) tl))

  let reverse s =
    let rec reverse' acc s =
      lazy (
        match s with
        | lazy Nil -> !$ acc
        | lazy (Cons (hd, tl)) -> !$ (reverse' (lazy (Cons (hd, acc))) tl))
    in
    reverse' (lazy Nil) s
end


open Stream

module BankersQueue : QUEUE = struct
  type 'a queue = int * 'a stream * int * 'a stream

  let empty = 0, lazy Nil, 0, lazy Nil
  let is_empty (lenf, _, _, _) = lenf = 0

  let check (lenf, f, lenr, r as q) =
    if lenr <= lenf then q
    else (lenf + lenr, f ++ reverse r, 0, lazy Nil)

  let snoc (lenf, f, lenr, r) x =
    check (lenf, f, lenr + 1, lazy (Cons (x, r)))

  let head = function
    | _, lazy Nil, _, _ -> raise Empty
    | _, lazy (Cons (x, _)), _, _ -> x

  let tail = function
    | _, lazy Nil, _, _ -> raise Empty
    | lenf, lazy (Cons (_, f')), lenr, r -> check (lenf - 1, f', lenr, r)
end


module LazyBinomialHeap (Element : ORDERED)
  : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type tree = Node of int * Elem.t * tree list
  type heap = tree list Lazy.t

  let empty = lazy []
  let is_empty ts = !$ts = []

  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rec ins_tree t ts = match t, ts with
    | _, [] -> [t]
    | t, t' :: ts' ->
        if rank t < rank t' then t :: ts
        else ins_tree (link t t') ts'

  let rec mrg ts1 ts2 = match ts1, ts2 with
    | _, [] -> ts1
    | [], _ -> ts2
    | t1 :: ts1', t2 :: ts2' ->
        if rank t1 < rank t2 then t1 :: mrg ts1' ts2
        else if rank t2 < rank t1 then t2 :: mrg ts1 ts2'
        else ins_tree (link t1 t2) (mrg ts1' ts2')

  (* fun lazy *)
  let insert x ts = lazy (ins_tree (Node (0, x, [])) !$ts)

  (* fun lazy *)
  let merge ts1 ts2 = lazy (mrg !$ts1 !$ts2)

  let rec remove_min_tree = function
    | [] -> raise Empty
    | [t] -> t, []
    | t :: ts ->
        let t', ts' = remove_min_tree ts in
        if Elem.leq (root t) (root t') then t, ts
        else t', t :: ts'

  let find_min ts = let t, _ = remove_min_tree !$ts in root t

  (* fun lazy *)
  let delete_min ts =
    let Node (_, _, ts1), ts2 = remove_min_tree !$ts in
    lazy (mrg (List.rev ts1) ts2)
end


module PhysicistsQueue : QUEUE = struct
  type 'a queue = 'a list * int * 'a list Lazy.t * int * 'a list

  let empty = [], 0, lazy [], 0, []
  let is_empty (_, lenf, _, _, _) = lenf = 0

  let checkw = function
    | [], lenf, f, lenr, r -> !$f, lenf, f, lenr, r
    | q -> q

  let check (w, lenf, f, lenr, r as q) =
    if lenr <= lenf then checkw q
    else
      let f' = !$f in
      checkw (f', lenf + lenr, lazy (f' @ List.rev r), 0, [])

  let snoc (w, lenf, f, lenr, r) x = check (w, lenf, f, lenr + 1, x :: r)

  let head = function
    | [], _, _, _, _ -> raise Empty
    | x :: _, _, _, _, _ -> x

  let tail = function
    | [], _, _, _, _ -> raise Empty
    | x :: w, lenf, f, lenr, r ->
        check (w, lenf - 1, lazy (List.tl !$f), lenr, r)
end


module type SORTABLE = sig
  module Elem : ORDERED

  type sortable

  val empty : sortable
  val add : Elem.t -> sortable -> sortable
  val sort : sortable -> Elem.t list
end


module BottomUpMergeSort (Element : ORDERED)
  : (SORTABLE with module Elem = Element) =
struct
  module Elem = Element

  type sortable = int * Elem.t list list Lazy.t

  let rec mrg xs ys = match xs, ys with
    | [], _ -> ys
    | _, [] -> xs
    | x :: xs', y :: ys' ->
        if Elem.leq x y then x :: mrg xs' ys
        else y :: mrg xs ys'

  let empty = 0, lazy []

  let add x (size, segs) =
    let rec add_seg seg size segs =
      if size mod 2 = 0 then seg :: segs
      else add_seg (mrg seg (List.hd segs)) (size / 2) (List.tl segs) in
    size + 1, lazy (add_seg [x] size !$segs)

  let sort (size, segs) =
    let rec mrg_all xs = function
      | [] -> xs
      | seg :: segs -> mrg_all (mrg xs seg) segs in
    mrg_all [] !$segs
end


module LazyPairingHeap (Element : ORDERED) : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type heap = E | T of Elem.t * heap * heap Lazy.t

  let empty = E
  let is_empty h = h = E

  let rec merge a b = match a, b with
    | _, E -> a
    | E, _ -> b
    | T (x, _, _), T (y, _, _) -> if Elem.leq x y then link a b else link b a

  and link h a = match h with
    | T (x, E, m) -> T (x, a, m)
    | T (x, b, m) -> T (x, E, lazy (merge (merge a b) !$m))
    | _ -> impossible_pat "link"

  let insert x a = merge (T (x, E, lazy E)) a

  let find_min = function E -> raise Empty | T (x, _, _) -> x
  let delete_min = function E -> raise Empty | T (_, a, b) -> merge a !$b
end
