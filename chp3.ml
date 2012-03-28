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
(*                              Chapter 3                              *)
(***********************************************************************)

exception Empty
exception Impossible_pattern of string

let impossible_pat x = raise (Impossible_pattern x)


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


module LeftistHeap (Element : ORDERED) : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type heap = E | T of int * Elem.t * heap * heap

  let rank = function E -> 0 | T (r,_,_,_) -> r

  let makeT x a b =
    if rank a >= rank b then T (rank b + 1, x, a, b)
    else T (rank a + 1, x, b, a)

  let empty = E
  let is_empty h = h = E

  let rec merge h1 h2 = match h1, h2 with
    | _, E -> h1
    | E, _ -> h2
    | T (_, x, a1, b1), T (_, y, a2, b2) ->
        if Elem.leq x y then makeT x a1 (merge b1 h2)
        else makeT y a2 (merge h1 b2)

  let insert x h = merge (T (1, x, E, E)) h
  let find_min = function E -> raise Empty | T (_, x, _, _) -> x
  let delete_min = function E -> raise Empty | T (_, x, a, b) -> merge a b
end


module BinomialHeap (Element : ORDERED) : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type tree = Node of int * Elem.t * tree list
  type heap = tree list

  let empty = []
  let is_empty ts = ts = []

  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rec ins_tree t = function
    | [] -> [t]
    | t' :: ts' as ts ->
        if rank t < rank t' then t :: ts
        else ins_tree (link t t') ts'

  let insert x ts = ins_tree (Node (0, x, [])) ts

  let rec merge ts1 ts2 = match ts1, ts2 with
    | _, [] -> ts1
    | [], _ -> ts2
    | t1 :: ts1', t2 :: ts2' ->
        if rank t1 < rank t2 then t1 :: merge ts1' ts2
        else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
        else ins_tree (link t1 t2) (merge ts1' ts2')

  let rec remove_min_tree = function
    | [] -> raise Empty
    | [t] -> t, []
    | t :: ts ->
        let t', ts' = remove_min_tree ts in
        if Elem.leq (root t) (root t') then (t, ts)
        else (t', t :: ts')

  let find_min ts = root (fst (remove_min_tree ts))

  let delete_min ts =
    let Node (_, x, ts1), ts2 = remove_min_tree ts in
    merge (List.rev ts1) ts2
end


module type SET = sig
  type elem
  type set

  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
end


module RedBlackSet (Element : ORDERED) : (SET with type elem = Element.t) =
struct
  type elem = Element.t

  type color = R | B
  type tree = E | T of color * tree * elem * tree
  type set = tree

  let empty = E

  let rec member x = function
    | E -> false
    | T (_, a, y, b) ->
        if Element.lt x y then member x a
        else if Element.lt y x then member x b
        else true

  let balance = function
    | B, T (R, T (R, a, x, b), y, c), z, d
    | B, T (R, a, x, T (R, b, y, c)), z, d
    | B, a, x, T (R, T (R, b, y, c), z, d)
    | B, a, x, T (R, b, y, T (R, c, z, d)) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))
    | a, b, c, d -> T (a, b, c, d)

  let insert x s =
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, a, y, b) as s ->
          if Element.lt x y then balance (color, ins a, y, b)
          else if Element.lt y x then balance (color, a, y, ins b)
          else s in
    match ins s with  (* guaranteed to be non-empty *)
    | T (_, a, y, b) -> T (B, a, y, b)
    | _ -> impossible_pat "insert"
end
