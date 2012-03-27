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
(*                              Chapter 8                              *)
(***********************************************************************)

exception Empty
exception Not_implemented
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

module HoodMelvilleQueue : QUEUE = struct
  type 'a rotation_state =
    | Idle
    | Reversing of int * 'a list * 'a list * 'a list * 'a list
    | Appending of int * 'a list * 'a list
    | Done of 'a list

  type 'a queue = int * 'a list * 'a rotation_state * int * 'a list

  let exec = function
    | Reversing (ok, x :: f, f', y :: r, r') ->
        Reversing (ok + 1, f, x :: f', r, y :: r')
    | Reversing (ok, [], f', [y], r') -> Appending (ok, f', y :: r')
    | Appending (0, _, r') -> Done r'
    | Appending (ok, x :: f', r') -> Appending (ok - 1, f', x :: r')
    | state -> state

  let invalidate = function
    | Reversing (ok, f, f', r, r') -> Reversing (ok - 1, f, f', r, r')
    | Appending (0, _, _ :: r') -> Done r'
    | Appending (ok, f', r') -> Appending (ok - 1, f', r')
    | state -> state

  let exec2 (lenf, f, state, lenr, r) =
    match exec (exec state) with
    | Done newf -> (lenf, newf, Idle, lenr, r)
    | newstate -> lenf, f, newstate, lenr, r

  let check ((lenf, f, state, lenr, r) as q) =
    if lenr <= lenf then exec2 q
    else
      let newstate = Reversing (0, f, [], r, []) in
      exec2 (lenf + lenr, f, newstate, 0, [])

  let empty = 0, [], Idle, 0, []
  let is_empty (lenf, _, _, _, _) = lenf = 0

  let snoc (lenf, f, state, lenr, r) x = check (lenf, f, state, lenr + 1, x::r)

  let head = function
    | lenf, [], state, lenr, r -> raise Empty
    | lenf, x :: f, state, lenr, r -> x

  let tail = function
    | lenf, [], state, lenr, r -> raise Empty
    | lenf, x :: f, state, lenr, r ->
        check (lenf - 1, f, invalidate state, lenr, r)
end


module BankersDeque (C : sig val c : int end) : DEQUE =  (* c > 1 *)
struct
  let c = C.c

  type 'a queue = int * 'a stream * int * 'a stream

  let empty = 0, lazy Nil, 0, lazy Nil
  let is_empty (lenf, _, lenr, _) = lenf + lenr = 0

  let check (lenf, f, lenr, r as q) =
    if lenf > c*lenr + 1 then
      let i = (lenf + lenr) / 2 in
      i, take i f, lenf + lenr - i, r ++ reverse (drop i f)
    else if lenr > c*lenf + 1 then
      let j = (lenf + lenr) / 2 in
      lenf + lenr - j, f ++ reverse (drop j r), j, take j r
    else q

  let cons x (lenf, f, lenr, r) = check (lenf + 1, lazy (Cons (x, f)), lenr, r)

  let head = function
    | _, lazy Nil, _, lazy Nil -> raise Empty
    | _, lazy Nil, _, lazy (Cons (x, _)) -> x
    | _, lazy (Cons (x, _)), _, _ -> x

  let tail = function
    | _, lazy Nil, _, lazy Nil -> raise Empty
    | _, lazy Nil, _, lazy (Cons (_, _)) -> empty
    | lenf, lazy (Cons (x, f')), lenr, r -> check (lenf - 1, f', lenr, r)

  let snoc (lenf, f, lenr, r) x = check (lenf, f, lenr + 1, lazy (Cons (x, r)))

  let last = function
    | _, lazy Nil, _, lazy Nil -> raise Empty
    | _, lazy (Cons (x, _)), _, lazy Nil -> x
    | _, _, _, lazy (Cons (x, _)) -> x

  let init = function
    | _, lazy Nil, _, lazy Nil -> raise Empty
    | _, lazy (Cons (_, _)), _, lazy Nil -> empty
    | lenf, f, lenr, lazy (Cons (_, r')) -> check (lenf, f, lenr - 1, r')
end


module RealTimeDeque (C : sig val c : int end) : DEQUE =  (* c = 2 or c = 3 *)
struct
  let c = C.c

  type 'a queue = int * 'a stream * 'a stream * int * 'a stream * 'a stream

  let empty = 0, lazy Nil, lazy Nil, 0, lazy Nil, lazy Nil
  let is_empty (lenf, f, sf, lenr, r, sr) = lenf + lenr = 0

  let exec1 = function lazy (Cons (_, s)) -> s | s -> s
  let exec2 s = exec1 (exec1 s)

  let rec rotate_rev s r a = match s, r, a with
    | lazy Nil, _, _ -> reverse r ++ a
    | lazy (Cons (x, f)), _, _ ->
        lazy (Cons (x, rotate_rev f (drop c r) (reverse (take c r) ++ a)))

  let rec rotate_drop f j r =
    if j < c then rotate_rev f (drop j r) (lazy Nil)
    else
      match f with
      | lazy (Cons (x, f')) ->
          lazy (Cons (x, rotate_drop f' (j - c) (drop c r)))
      | _ -> impossible_pat "rotate_drop"

  let check (lenf, f, sf, lenr, r, sr as q) =
    if lenf > c*lenr + 1 then
      let i = (lenf + lenr) / 2 in
      let f' = take i f
      and r' = rotate_drop r i f in
      i, f', f', lenf + lenr - i, r', r'
    else if lenr > c*lenf + 1 then
      let j = (lenf + lenr) / 2 in
      let r' = take j r
      and f' = rotate_drop f j r in
      lenf + lenr - j, f', f', j, r', r'
    else q

  let cons x (lenf, f, sf, lenr, r, sr) =
    check (lenf + 1, lazy (Cons (x, f)), exec1 sf, lenr, r, exec1 sr)

  let head = function
    | _, lazy Nil, _, _, lazy Nil, _ -> raise Empty
    | _, lazy Nil, _, _, lazy (Cons (x, _)), _ -> x
    | _, lazy (Cons (x, _)), _, _, _, _ -> x

  let tail = function
    | _, lazy Nil, _, _, lazy Nil, _ -> raise Empty
    | _, lazy Nil, _, _, lazy (Cons (x, _)), _ -> empty
    | lenf, lazy (Cons (x, f')), sf, lenr, r, sr ->
        check (lenf - 1, f', exec2 sf, lenr, r, exec2 sr)

  let snoc (lenf, f, sf, lenr, r, sr) x =
    check (lenf, f, exec1 sf, lenr + 1, lazy (Cons (x, r)), exec1 sr)

  let last = function
    | _, lazy Nil, _, _, lazy Nil, _ -> raise Empty
    | _, lazy (Cons (x, _)), _, _, lazy Nil, _ -> x
    | _, _, _, _, lazy (Cons (x, _)), _ -> x

  let init = function
    | _, lazy Nil, _, _, lazy Nil, _ -> raise Empty
    | _, lazy (Cons (x, _)), _, _, lazy Nil, _ -> empty
    | lenf, f, sf, lenr, lazy (Cons (x, r')), sr ->
        check (lenf, f, exec2 sf, lenr - 1, r', exec2 sr)
end
