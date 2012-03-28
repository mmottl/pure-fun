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

(************************************************************************)
(*                              Chapter 9                               *)
(************************************************************************)

exception Empty
exception Subscript
exception Impossible_pattern of string

let impossible_pat x = raise (Impossible_pattern x)


module Dense = struct
  type digit = Zero | One
  type nat = digit list  (* increasing order of significance *)

  let rec inc = function
    | [] -> [One]
    | Zero :: ds -> One :: ds
    | One :: ds -> Zero :: inc ds  (* carry *)

  let rec dec = function
    | [One] -> []
    | One :: ds -> Zero :: ds
    | Zero :: ds -> One :: dec ds  (* borrow *)
    | [] -> impossible_pat "dec"

  let rec add d1 d2 = match d1, d2 with
    | ds, [] -> ds
    | [], ds -> ds
    | d :: ds1, Zero :: ds2 -> d :: add ds1 ds2
    | Zero :: ds1, d :: ds2 -> d :: add ds1 ds2
    | One :: ds1, One :: ds2 -> Zero :: inc (add ds1 ds2)  (* carry *)
end


module SparseByWeight = struct
  type nat = int list  (* increasing list of weights, each a power of two *)

  let rec carry w = function
    | [] -> [w]
    | w' :: ws' as ws -> if w < w' then w :: ws else carry (2*w) ws'

  let rec borrow w = function
    | w' :: ws' as ws -> if w = w' then ws' else w :: borrow (2*w) ws
    | [] -> impossible_pat "borrow"

  let inc ws = carry 1 ws
  let dec ws = borrow 1 ws

  let rec add m n = match m, n with
    | _, [] -> m
    | [], _ -> n
    | w1 :: ws1, w2 :: ws2 ->
        if w1 < w2 then w1 :: add ws1 n
        else if w2 < w1 then w2 :: add m ws2
        else carry (2*w1) (add ws1 ws2)
end


module type RANDOM_ACCESS_LIST = sig
  type 'a ra_list

  val empty : 'a ra_list
  val is_empty : 'a ra_list -> bool

  val cons : 'a -> 'a ra_list -> 'a ra_list
  val head : 'a ra_list -> 'a
  val tail : 'a ra_list -> 'a ra_list
    (* head and tail raise Empty if list is empty *)

  val lookup : int -> 'a ra_list -> 'a
  val update : int -> 'a -> 'a ra_list -> 'a ra_list
    (* lookup and update raise Subscript if index is out of bounds *)
end


module BinaryRandomAccessList : RANDOM_ACCESS_LIST = struct
  type 'a tree = Leaf of 'a | Node of int * 'a tree * 'a tree
  type 'a digit = Zero | One of 'a tree
  type 'a ra_list = 'a digit list

  let empty = []
  let is_empty ts = ts = []

  let size = function
    | Leaf x -> 1
    | Node (w, _, _) -> w

  let link t1 t2 = Node (size t1 + size t2, t1, t2)

  let rec cons_tree t = function
    | [] -> [One t]
    | Zero :: ts -> One t :: ts
    | One t' :: ts -> Zero :: cons_tree (link t t') ts

  let rec uncons_tree = function
    | [] -> raise Empty
    | [One t] -> t, []
    | One t :: ts -> t, Zero :: ts
    | Zero :: ts ->
        match uncons_tree ts with
        | Node (_, t1, t2), ts' -> t1, One t2 :: ts'
        | _ -> impossible_pat "uncons_tree"

  let cons x ts = cons_tree (Leaf x) ts

  let head ts =
    match uncons_tree ts with
    | Leaf x, _ -> x
    | _ -> impossible_pat "head"

  let tail ts = snd (uncons_tree ts)

  let rec lookup_tree i t = match i, t with
    | 0, Leaf x -> x
    | i, Leaf x -> raise Subscript
    | i, Node (w, t1, t2) ->
        if i < w/2 then lookup_tree i t1
        else lookup_tree (i - w/2) t2

  let rec update_tree i y t = match i, t with
    | 0, Leaf x -> Leaf y
    | _, Leaf x -> raise Subscript
    | _, Node (w, t1, t2) ->
        if i < w/2 then Node (w, update_tree i y t1, t2)
        else Node (w, t1, update_tree (i - w/2) y t2)

  let rec lookup i = function
    | [] -> raise Subscript
    | Zero :: ts -> lookup i ts
    | One t :: ts ->
        if i < size t then lookup_tree i t
        else lookup (i - size t) ts

  let rec update i y = function
    | [] -> raise Subscript
    | Zero :: ts -> Zero :: update i y ts
    | One t :: ts ->
        if i < size t then One (update_tree i y t) :: ts
        else One t :: update (i - size t) y ts
end


module SkewBinaryRandomAccessList : RANDOM_ACCESS_LIST = struct
  type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
  type 'a ra_list = (int * 'a tree) list (* integer is the weight of the tree *)

  let empty = []
  let is_empty ts = ts = []

  let cons x = function
    | (w1, t1) :: (w2, t2) :: ts' as ts ->
        if w1 = w2 then (1 + w1 + w2, Node (x, t1, t2)) :: ts'
        else (1, Leaf x) :: ts
    | ts -> (1, Leaf x) :: ts

  let head = function
    | [] -> raise Empty
    | (1, Leaf x) :: _ -> x
    | (_, Node (x, _, _)) :: _ -> x
    | _ -> impossible_pat "head"

  let tail = function
    | [] -> raise Empty
    | (1, Leaf _) :: ts -> ts
    | (w, Node (x, t1, t2)) :: ts -> (w/2, t1) :: (w/2, t2) :: ts
    | _ -> impossible_pat "tail"

  let rec lookup_tree w i t = match w, i, t with
    | 1, 0, Leaf x -> x
    | 1, _, Leaf x -> raise Subscript
    | _, 0, Node (x, t1, t2) -> x
    | _, _, Node (x, t1, t2) ->
        if i <= w/2 then lookup_tree (w/2) (i - 1) t1
        else lookup_tree (w/2) (i - 1 - w/2) t2
    | _ -> impossible_pat "lookup_tree"

  let rec update_tree = function
    | 1, 0, y, Leaf x -> Leaf y
    | 1, i, y, Leaf x -> raise Subscript
    | w, 0, y, Node (x, t1, t2) -> Node (y, t1, t2)
    | w, i, y, Node (x, t1, t2) ->
        if i <= w/2 then Node (x, update_tree (w/2, i - 1, y, t1), t2)
        else Node (x, t1, update_tree (w/2, i - 1 - w/2, y, t2))
    | _ -> impossible_pat "update_tree"

  let rec lookup i = function
    | [] -> raise Subscript
    | (w, t) :: ts ->
        if i < w then lookup_tree w i t
        else lookup (i - w) ts

  let rec update i y = function
    | [] -> raise Subscript
    | (w, t) :: ts ->
        if i < w then (w, update_tree (w, i, y, t)) :: ts
        else (w, t) :: update (i - w) y ts
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


module SkewBinomialHeap (Element : ORDERED)
  : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type tree = Node of int * Elem.t * Elem.t list * tree list
  type heap = tree list

  let empty = []
  let is_empty ts = ts = []

  let rank (Node (r, _, _, _)) = r
  let root (Node (_, x, _, _)) = x

  let link (Node (r, x1, xs1, c1) as t1) (Node (_, x2, xs2, c2) as t2) =
    if Elem.leq x1 x2 then Node (r + 1, x1, xs1, t2 :: c1)
    else Node (r + 1, x2, xs2, t1 :: c2)

  let skew_link x t1 t2 =
    let Node (r, y, ys, c) = link t1 t2 in
    if Elem.leq x y then Node (r, x, y :: ys, c)
    else Node (r, y, x :: ys, c)

  let rec ins_tree t = function
    | [] -> [t]
    | t' :: ts ->
        if rank t < rank t' then t :: t' :: ts
        else ins_tree (link t t') ts

  let rec merge_trees ts1 ts2 = match ts1, ts2 with
    | _, [] -> ts1
    | [], _ -> ts2
    | t1 :: ts1', t2 :: ts2' ->
        if rank t1 < rank t2 then t1 :: merge_trees ts1' ts2
        else if rank t2 < rank t1 then t2 :: merge_trees ts1 ts2'
        else ins_tree (link t1 t2) (merge_trees ts1' ts2')

  let normalize = function
    | [] -> []
    | t :: ts -> ins_tree t ts

  let insert x = function
    | t1 :: t2 :: rest as ts ->
        if rank t1 = rank t2 then skew_link x t1 t2 :: rest
        else Node (0, x, [], []) :: ts
    | ts -> Node (0, x, [], []) :: ts

  let merge ts1 ts2 = merge_trees (normalize ts1) (normalize ts2)

  let rec remove_min_tree = function
    | [] -> raise Empty
    | [t] -> t, []
    | t :: ts ->
        let t', ts' = remove_min_tree ts in
        if Elem.leq (root t) (root t') then t, ts else t', t :: ts'

  let find_min ts = root (fst (remove_min_tree ts))

  let delete_min ts =
    let Node (_, x, xs, ts1), ts2 = remove_min_tree ts in
      let rec insert_all ts = function
        | [] -> ts
        | x :: xs' -> insert_all (insert x ts) xs' in
    insert_all (merge (List.rev ts1) ts2) xs
end
