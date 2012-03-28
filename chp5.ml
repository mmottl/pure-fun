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
(*                              Chapter 5                              *)
(***********************************************************************)

exception Empty


module type QUEUE = sig
  type 'a queue

  val empty : 'a queue
  val is_empty : 'a queue -> bool

  val snoc : 'a queue -> 'a -> 'a queue
  val head : 'a queue -> 'a        (* raises Empty if queue is empty *)
  val tail : 'a queue -> 'a queue  (* raises Empty if queue is empty *)
end


module BatchedQueue : QUEUE = struct
  type 'a queue = 'a list * 'a list

  let empty = [], []
  let is_empty (f, r) = f = []

  let checkf (f, r as q) = if f = [] then List.rev r, f else q

  let snoc (f, r) x = checkf (f, x :: r)
  let head = function [], _ -> raise Empty | x :: _, _ -> x
  let tail = function [], _ -> raise Empty | _ :: f, r -> checkf (f, r)
end


module type DEQUE = sig
  type 'a queue

  val empty : 'a queue
  val is_empty : 'a queue -> bool

  (* insert, inspect, and remove the front element *)
  val cons : 'a -> 'a queue -> 'a queue
  val head : 'a queue -> 'a        (* raises Empty if queue is empty *)
  val tail : 'a queue -> 'a queue  (* raises Empty if queue is empty *)

  (* insert, inspect, and remove the rear element *)
  val snoc : 'a queue -> 'a -> 'a queue
  val last : 'a queue -> 'a        (* raises Empty if queue is empty *)
  val init : 'a queue -> 'a queue  (* raises Empty if queue is empty *)
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


module SplayHeap (Element : ORDERED) : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type heap = E | T of heap * Elem.t * heap

  let empty = E
  let is_empty h = h = E

  let rec partition pivot = function
    | E -> E, E
    | T (a, x, b) as t ->
        if Elem.leq x pivot then
          match b with
          | E -> t, E
          | T (b1, y, b2) ->
              if Elem.leq y pivot then
                let small, big = partition pivot b2 in
                T (T (a, x, b1), y, small), big
              else
                let small, big = partition pivot b1 in
                T (a, x, small), T (big, y, b2)
        else
          match a with
          | E -> E, t
          | T (a1, y, a2) ->
              if Elem.leq y pivot then
                let small, big = partition pivot a2 in
                T (a1, y, small), T (big, x, b)
              else
                let small, big = partition pivot a1 in
                small, T (big, y, T (a2, x, b))

  let insert x t = let a, b = partition x t in T (a, x, b)

  let rec merge s t = match s, t with
    | E, _ -> t
    | T (a, x, b), _ ->
        let ta, tb = partition x t in
        T (merge ta a, x, merge tb b)

  let rec find_min = function
    | E -> raise Empty
    | T (E, x, _) -> x
    | T (a, x, _) -> find_min a

  let rec delete_min = function
    | E -> raise Empty
    | T (E, _, b) -> b
    | T (T (E, _, b), y, c) -> T (b, y, c)
    | T (T (a, x, b), y, c) -> T (delete_min a, x, T (b, y, c))
end


module PairingHeap (Element : ORDERED) : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type heap = E | T of Elem.t * heap list

  let empty = E
  let is_empty h = h = E

  let merge h1 h2 = match h1, h2 with
    | _, E -> h1
    | E, _ -> h2
    | T (x, hs1), T (y, hs2) ->
        if Elem.leq x y then T (x, h2 :: hs1)
        else T (y, h1 :: hs2)

  let insert x h = merge (T (x, [])) h

  let rec merge_pairs = function
    | [] -> E
    | [h] -> h
    | h1 :: h2 :: hs -> merge (merge h1 h2) (merge_pairs hs)

  let find_min = function
    | E -> raise Empty
    | T (x, hs) -> x

  let delete_min = function
    | E -> raise Empty
    | T (x, hs) -> merge_pairs hs
end
