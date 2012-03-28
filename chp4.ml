(*
   Original source code in SML from:

     Purely Functional Data Structures
     Chris Okasaki
     Cambridge University Press, 1998
     Copyright (c) 1998 Cambridge University Press

   Translation from SML to OCAML (this file):

     Copyright (C) 1999 - 2012  Markus Mottl
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
(*                              Chapter 4                              *)
(***********************************************************************)

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

(* MM: for demonstration purposes *)
(*
open Stream

let rec l_map f s =
  lazy (
    match s with
    | lazy Nil -> Nil
    | lazy (Cons (hd, tl)) -> Cons (f hd, l_map f tl))

let rec l_iter f n = function
  | lazy (Cons (hd, tl)) when n > 0 -> f hd; l_iter f (n - 1) tl
  | _ -> ()

let rec nat = lazy (Cons (0, l_map succ nat))

let _ =
  let test = reverse (take 10 (drop 50 (take 1_000_000_000 nat))) in
  l_iter (fun n -> Printf.printf "%d\n" n) 1_000 test
*)
