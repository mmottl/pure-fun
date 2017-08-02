#!/usr/bin/env ocaml

#use "topfind"
#require "topkg"

open Topkg

let () =
  let opams = [
    Pkg.opam_file ~lint_deps_excluding:None ~install:false "pure-fun.opam"
  ] in
  Pkg.describe ~opams ~metas:[] "pure-fun" @@ fun _c ->
  Ok [ Pkg.nothing ]
