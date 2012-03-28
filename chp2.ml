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
(*                              Chapter 2                              *)
(***********************************************************************)

exception Empty
exception Subscript


module type STACK = sig
  type 'a stack

  val empty : 'a stack
  val is_empty : 'a stack -> bool
  val cons : 'a -> 'a stack -> 'a stack
  val head : 'a stack -> 'a        (* raises Empty if stack is empty *)
  val tail : 'a stack -> 'a stack  (* raises Empty if stack is empty *)
end


module ListStack : STACK = struct
  type 'a stack = 'a list

  let empty = []
  let is_empty s = s = []
  let cons x s = x :: s
  let head = function [] -> raise Empty | h :: _ -> h
  let tail = function [] -> raise Empty | _ :: t -> t
end


module CustomStack : STACK = struct
  type 'a stack = Nil | Cons of 'a * 'a stack

  let cons x s = Cons (x, s)
  let empty = Nil

  let is_empty s = s = Nil
  let head = function Nil -> raise Empty | Cons (x, _) -> x
  let tail = function Nil -> raise Empty | Cons (_, s) -> s

  let rec (++) xs ys =
    if is_empty xs then ys
    else cons (head xs) (tail xs ++ ys)
end


let rec (++) xs ys = match xs with
  | [] -> ys
  | xh :: xt -> xh :: (xt ++ ys)

let rec update lst i y = match lst, i with
  | [], _ -> raise Subscript
  | x :: xs, 0 -> y :: xs
  | x :: xs, _ -> x :: update xs (i - 1) y


module type SET = sig
  type elem
  type set

  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
end


(* A totally ordered type and its comparison functions *)
module type ORDERED = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end


module UnbalancedSet (Element : ORDERED) : (SET with type elem = Element.t) =
struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set = tree

  let empty = E

  let rec member x = function
    | E -> false
    | T (a, y, b) ->
        if Element.lt x y then member x a
        else if Element.lt y x then member x b
        else true

  let rec insert x = function
    | E -> T (E, x, E)
    | T (a, y, b) as s ->
        if Element.lt x y then T (insert x a, y, b)
        else if Element.lt y x then T (a, y, insert x b)
        else s
end


module type FINITE_MAP = sig
  type key
  type 'a map

  val empty : 'a map
  val bind : key -> 'a -> 'a map -> 'a map
  val lookup : key -> 'a map -> 'a  (* raise Not_found if key is not found *)
end
