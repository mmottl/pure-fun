(*
   Original source code in SML from:

     Purely Functional Data Structures
     Chris Okasaki
     Cambridge University Press, 1998
     Copyright (c) 1998 Cambridge University Press

   Translation from SML to OCAML (this file):

     Copyright (C) 1999 - 2002  Markus Mottl
     email:  markus.mottl@gmail.com
     www:    http://www.ocaml.info

   Unless this violates copyrights of the original sources, the following
   licence applies to this file:

   This source code is free software; you can redistribute it and/or
   modify it without any restrictions. It is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY.
*)

(***********************************************************************)
(*                              Chapter 4                              *)
(***********************************************************************)

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
  let rec take n = function
    | _ when n = 0 -> Nil
    | Nil -> Nil
    | Cons (hd, tl) -> Cons (hd, lazy (take (n - 1) !$tl))

  (* function lazy *)
  let rec drop n = function
    | s when n = 0 -> s
    | Nil -> Nil
    | Cons (_, tl) -> drop (n - 1) !$tl

  (* function lazy *)
  let reverse s =
    let rec reverse' acc = function
      | Nil -> acc
      | Cons (hd, tl) -> reverse' (Cons (hd, lazy acc)) !$tl in
    reverse' Nil s
end

(*
(* MM: for demonstration purposes *)
open Stream

let rec l_map f = function
  | Nil -> Nil
  | Cons (hd, tl) -> Cons (f hd, lazy (l_map f !$tl))

let rec l_iter f n = function
  | Nil -> ()
  | Cons (hd, tl) -> if n > 0 then begin f hd; l_iter f (n-1) !$tl end

let rec nat = Cons (0, lazy (l_map succ nat))

let _ =
  let test = reverse (take 10 (drop 50 (take 1000000000 nat))) in
  l_iter (fun n -> print_int n; print_newline ()) 1000 test
*)
