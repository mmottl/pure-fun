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
(*                              Chapter 7                              *)
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


module type SORTABLE = sig
  module Elem : ORDERED

  type sortable

  val empty : sortable
  val add : Elem.t -> sortable -> sortable
  val sort : sortable -> Elem.t list
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

module RealTimeQueue : QUEUE = struct
  type 'a queue = 'a stream * 'a list * 'a stream

  let empty = lazy Nil, [], lazy Nil

  let is_empty = function lazy Nil, _, _ -> true | _ -> false

  let rec rotate = function
    | lazy Nil, y :: _, a -> lazy (Cons (y, a))
    | lazy (Cons (x, xs)), y :: ys, a ->
        lazy (Cons (x, rotate (xs, ys, lazy (Cons (y, a)))))
    | _, [], _ -> impossible_pat "rotate"

  let exec = function
    | f, r, lazy (Cons (x, s)) -> f, r, s
    | f, r, lazy Nil -> let f' = rotate (f, r, lazy Nil) in f', [], f'

  let snoc (f, r, s) x = exec (f, x :: r, s)

  let head (f, _, _) = match f with
    | lazy Nil -> raise Empty
    | lazy (Cons (x, _)) -> x

  let tail = function
    | lazy Nil, _, _ -> raise Empty
    | lazy (Cons (_, f)), r, s -> exec (f, r, s)
end


let rec list_to_stream = function
  | [] -> lazy Nil
  | x :: xs -> lazy (Cons (x, list_to_stream xs))


module ScheduledBinomialHeap (Element : ORDERED)
  : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type tree = Node of Elem.t * tree list
  type digit = Zero | One of tree
  type schedule = digit stream list
  type heap = digit stream * schedule

  let empty = lazy Nil, []
  let is_empty (ds, _) = ds = lazy Nil

  let link (Node (x1, c1) as t1) (Node (x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (x1, t2 :: c1)
    else Node (x2, t1 :: c2)

  let rec ins_tree t = function
    | lazy Nil -> lazy (Cons (One t, lazy Nil))
    | lazy (Cons (Zero, ds)) -> lazy (Cons (One t, ds))
    | lazy (Cons (One t', ds)) -> lazy (Cons (Zero, ins_tree (link t t') ds))

  let rec mrg a b = match a, b with
    | ds1, lazy Nil -> ds1
    | lazy Nil, ds2 -> ds2
    | lazy (Cons (Zero, ds1)), lazy (Cons (d, ds2)) ->
        lazy (Cons (d, mrg ds1 ds2))
    | lazy (Cons (d, ds1)), lazy (Cons (Zero, ds2)) ->
        lazy (Cons (d, mrg ds1 ds2))
    | lazy (Cons (One t1, ds1)), lazy (Cons (One t2, ds2)) ->
        lazy (Cons (Zero, ins_tree (link t1 t2) (mrg ds1 ds2)))

  let rec normalize ds = match ds with
    | lazy Nil -> ds
    | lazy (Cons (_, ds')) -> ignore (normalize ds'); ds

  let exec = function
    | [] -> []
    | lazy (Cons (Zero, job)) :: sched -> job :: sched
    | _ :: sched -> sched

  let insert x (ds, sched) =
    let ds' = ins_tree (Node (x, [])) ds in
    ds', exec (exec (ds' :: sched))

  let merge (ds1, _) (ds2, _) = normalize (mrg ds1 ds2), []

  let rec remove_min_tree = function
    | lazy Nil -> raise Empty
    | lazy (Cons (hd, tl)) ->
        match hd, tl with
        | One t, lazy Nil -> t, lazy Nil
        | Zero, ds ->
            let t', ds' = remove_min_tree ds in
            t', lazy (Cons (Zero, ds'))
        | One (Node (x, _) as t), ds ->
            let Node (x', _) as t', ds' = remove_min_tree ds in
            if Elem.leq x x' then t, lazy (Cons (Zero, tl))
            else t', lazy (Cons (One t, ds'))

  let find_min (ds, _) = let Node (x, _), _ = remove_min_tree ds in x

  let delete_min (ds, _) =
    let Node (_, c), ds' = remove_min_tree ds in
    let ds'' =
      mrg (list_to_stream (List.map (fun e -> One e) (List.rev c))) ds' in
    normalize ds'', []
end


let rec stream_to_list = function
  | lazy Nil -> []
  | lazy (Cons (x, xs)) -> x :: stream_to_list xs


module ScheduledBottomUpMergeSort (Element : ORDERED)
  : (SORTABLE with module Elem = Element) =
struct
  module Elem = Element

  type schedule = Elem.t stream list
  type sortable = int * (Elem.t stream * schedule) list

  (* fun lazy *)
  let rec mrg xs ys = match xs, ys with
    | lazy Nil, _ -> ys
    | _, lazy Nil -> xs
    | lazy (Cons (x, xs')), lazy (Cons (y, ys')) ->
        if Elem.leq x y then lazy (Cons (x, mrg xs' ys))
        else lazy (Cons (y, mrg xs ys'))

  let rec exec1 = function
    | [] -> []
    | lazy Nil :: sched -> exec1 sched
    | lazy (Cons (x, xs)) :: sched -> xs :: sched

  let exec2 (xs, sched) = xs, exec1 (exec1 sched)

  let empty = 0, []

  let add x (size, segs) =
    let rec add_seg xs segs size rsched =
      if size mod 2 = 0 then (xs, List.rev rsched) :: segs
      else
        match segs with
        | (xs', []) :: segs' ->
            let xs'' = mrg xs xs' in
            add_seg xs'' segs' (size / 2) (xs'' :: rsched)
        | _ -> impossible_pat "add" in
    let segs' = add_seg (lazy (Cons (x, lazy Nil))) segs size [] in
    size + 1, List.map exec2 segs'

  let sort (size, segs) =
    let rec mrg_all = function
      | xs, [] -> xs
      | xs, (xs', _) :: segs -> mrg_all (mrg xs xs', segs) in
    stream_to_list (mrg_all (lazy Nil, segs))
end
