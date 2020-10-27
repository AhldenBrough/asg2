(* $Id: interp.mli,v 1.8 2020-10-22 19:59:13-07 - - $ *)

(*
* Interpreter for Mini Basic
*)

val want_dump : bool ref

val interpret_program : Absyn.program -> unit

