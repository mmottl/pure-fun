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

   Unless this violates copyrights of the original sources, the following
   licence applies to this file:

   This source code is free software; you can redistribute it and/or
   modify it without any restrictions. It is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY.
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
  type 'a stream = Nil | Cons of 'a * 'a stream Lazy.t

  val (++) : 'a stream -> 'a stream -> 'a stream  (* stream append *)
  val take : int -> 'a stream -> 'a stream
  val drop : int -> 'a stream -> 'a stream
  val reverse : 'a stream -> 'a stream
end

module Stream : STREAM = struct
  type 'a stream = Nil | Cons of 'a * 'a stream Lazy.t

  (* function lazy *)
  let rec (++) s1 s2 = match s1 with
    | Nil -> s2
    | Cons (hd, tl) -> Cons (hd, lazy (!$tl ++ s2))

  (* function lazy *)
  let rec take n s = match n, s with
    | 0, _ -> Nil
    | _, Nil -> Nil
    | _, Cons (hd, tl) -> Cons (hd, lazy (take (n - 1) !$tl))

  (* function lazy *)
  let drop n s =
    let rec drop' n s = match n, s with
      | 0, _ -> s
      | _, Nil -> Nil
      | _, Cons (_, tl) -> drop' (n - 1) !$tl in
    drop' n s

  (* function lazy *)
  let reverse s =
    let rec reverse' acc = function
      | Nil -> acc
      | Cons (hd, tl) -> reverse' (Cons (hd, lazy acc)) !$tl in
    reverse' Nil s
end


open Stream

module RealTimeQueue : QUEUE = struct
  type 'a queue = 'a stream * 'a list * 'a stream

  let empty = Nil, [], Nil

  let is_empty = function Nil, _, _ -> true | _ -> false

  let rec rotate = function
    | Nil, y :: _, a -> Cons (y, lazy a)
    | Cons (x, xs), y :: ys, a ->
        Cons (x, lazy (rotate (!$xs, ys, Cons (y, lazy a))))
    | _, [], _ -> impossible_pat "rotate"

  let exec = function
    | f, r, Cons (x, s) -> f, r, !$s
    | f, r, Nil -> let f' = rotate (f, r, Nil) in f', [], f'

  let snoc (f, r, s) x = exec (f, x :: r, s)

  let head (f, _, _) = match f with
    | Nil -> raise Empty
    | Cons (x, _) -> x

  let tail = function
    | Nil, _, _ -> raise Empty
    | Cons (_, f), r, s -> exec (!$f, r, s)
end


let rec list_to_stream = function
  | [] -> Nil
  | x :: xs -> Cons (x, lazy (list_to_stream xs))


module ScheduledBinomialHeap (Element : ORDERED)
  : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type tree = Node of Elem.t * tree list
  type digit = Zero | One of tree
  type schedule = digit stream list
  type heap = digit stream * schedule

  let empty = Nil, []
  let is_empty (ds, _) = ds = Nil

  let link (Node (x1, c1) as t1) (Node (x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (x1, t2 :: c1)
    else Node (x2, t1 :: c2)

  let rec ins_tree t = function
    | Nil -> Cons (One t, lazy Nil)
    | Cons (Zero, ds) -> Cons (One t, ds)
    | Cons (One t', ds) -> Cons (Zero, lazy (ins_tree (link t t') !$ds))

  let rec mrg a b = match a, b with
    | ds1, Nil -> ds1
    | Nil, ds2 -> ds2
    | Cons (Zero, ds1), Cons (d, ds2) -> Cons (d, lazy (mrg !$ds1 !$ds2))
    | Cons (d, ds1), Cons (Zero, ds2) -> Cons (d, lazy (mrg !$ds1 !$ds2))
    | Cons (One t1, ds1), Cons (One t2, ds2) ->
        Cons (Zero, lazy (ins_tree (link t1 t2) (mrg !$ds1 !$ds2)))

  let rec normalize ds = match ds with
    | Nil -> ds
    | Cons (_, ds') -> normalize (!$ds'); ds

  let exec = function
    | [] -> []
    | Cons (Zero, job) :: sched -> !$job :: sched
    | _ :: sched -> sched

  let insert x (ds, sched) =
    let ds' = ins_tree (Node (x, [])) ds in
    ds', exec (exec (ds' :: sched))

  let merge (ds1, _) (ds2, _) = normalize (mrg ds1 ds2), []

  let rec remove_min_tree = function
    | Nil -> raise Empty
    | Cons (hd, tl) ->
        match hd, !$tl with
        | One t, Nil -> t, Nil
        | Zero, ds ->
            let t', ds' = remove_min_tree ds in t', Cons (Zero, lazy ds')
        | One (Node (x, _) as t), ds ->
            let Node (x', _) as t', ds' = remove_min_tree ds in
            if Elem.leq x x' then t, Cons (Zero, tl)
            else t', Cons (One t, lazy ds')

  let find_min (ds, _) = let Node (x, _), _ = remove_min_tree ds in x

  let delete_min (ds, _) =
    let Node (_, c), ds' = remove_min_tree ds in
    let ds'' =
      mrg (list_to_stream (List.map (fun e -> One e) (List.rev c))) ds' in
    normalize ds'', []
end


let rec stream_to_list = function
  | Nil -> []
  | Cons (x, xs) -> x :: stream_to_list !$xs


module ScheduledBottomUpMergeSort (Element : ORDERED)
  : (SORTABLE with module Elem = Element) =
struct
  module Elem = Element

  type schedule = Elem.t stream list
  type sortable = int * (Elem.t stream * schedule) list

  (* fun lazy *)
  let rec mrg xs ys = match xs, ys with
    | Nil, _ -> ys
    | _, Nil -> xs
    | Cons (x, xs'), Cons (y, ys') ->
        if Elem.leq x y then Cons (x, lazy (mrg !$xs' ys))
        else Cons (y, lazy (mrg xs !$ys'))

  let rec exec1 = function
    | [] -> []
    | Nil :: sched -> exec1 sched
    | Cons (x, xs) :: sched -> !$xs :: sched

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
    let segs' = add_seg ((Cons (x, lazy Nil))) segs size [] in
    size + 1, List.map exec2 segs'

  let sort (size, segs) =
    let rec mrg_all = function
      | xs, [] -> xs
      | xs, (xs', _) :: segs -> mrg_all (mrg xs xs', segs) in
    stream_to_list (mrg_all (Nil, segs))
end
