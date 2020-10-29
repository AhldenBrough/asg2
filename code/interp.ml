(* $Id: interp.ml,v 1.16 2020-10-21 19:39:19-07 - - $ *)

(*
    So far:
    -31-collatz.mb, 40-sort-array, 41-eratosthenes do not work
    -could possibly be because of my dim?
*)

open Absyn

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> eval_memref memref
    | Unary (oper, expr1) -> Hashtbl.find Tables.unary_fn_table 
                            oper (eval_expr expr1)
    | Binary (oper, expr1, expr2) -> Hashtbl.find Tables.binary_fn_table 
                            oper (eval_expr expr1) (eval_expr expr2)

(*  The Goal with eval_memref when its an Arrayref:
    -we want to look up the ident in the array table since ident is the
     name of the array, or the identifier of the array
    -then we want to call eval-expr on the expr to get the position of
     the array we want to access
 *)

and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> Array.get 
                        (Hashtbl.find Tables.array_table ident)
                        (Etc.int_of_round_float (eval_expr expr))
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0

and eval_STUB reason = (
    print_string ("(" ^ reason ^ ")");
    nan)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continue -> match firstline with
      | _, _, None -> interpret continue
      | _, _, Some stmt -> (interp_stmt stmt continue)

and interp_stmt (stmt : Absyn.stmt) (continue : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim ident expr continue
    | Let (memref, expr) -> interp_let memref expr continue
    | Goto label -> interp_goto label
    | If (expr, label) -> interp_if expr label continue
    | Print print_list -> interp_print print_list continue
    | Input memref_list -> interp_input memref_list continue




(*  there are two cases of the let statement:
    -memref is just a variable -> (let a <some_expression>)
        -then we want to just store the key value pair into the var-table
    -memref is an array reference -> (let (asub arr pos) <some_expression>)
        -then we want to look up the array in the array-table, then set the 
         correct key value pair at the given positions
 *)
and interp_let (memref : Absyn.memref) (expr : Absyn.expr) (continue : Absyn.program) =
    match memref with
    | Arrayref (ident, pos) -> Array.set (Hashtbl.find Tables.array_table ident) 
                                        (Etc.int_of_round_float (eval_expr pos)) 
                                        (eval_expr expr)
    | Variable ident -> Hashtbl.add Tables.variable_table ident (eval_expr expr);

    interpret continue



(*  GOAL with interp_dim:
    -put in the array table a key value pair where the pair's format is:
        -(ident, (eval_expr expr))
*)
and interp_dim (ident: Absyn.ident) (expr: Absyn.expr) (continue : Absyn.program) =
    Hashtbl.add Tables.array_table 
        ident 
        (Array.make (Etc.int_of_round_float (eval_expr expr)) 0.0);
    (* print_hash Tables.array_table *)
    interpret continue



(*  The goal for goto is:
    -call interpret_program on label_table.get(label)
*)
and interp_goto (label: Absyn.label) =
    interpret (Hashtbl.find Tables.label_table label)



(*  The goal for if is:
    -evaluate the expression, if it is true, then call interp_goto on the label
    -otherwise, if the expression is not true, then "interpret continue"
*)
and interp_if (expr : Absyn.relexpr) (label : Absyn.label) (continue : Absyn.program) =
    match expr with
    | Relexpr (oper, expr1, expr2) -> 
        try 
            let actual_oper = Hashtbl.find Tables.bool_fn_table oper in
                if actual_oper (eval_expr expr1) (eval_expr expr2)
                    then interpret (Hashtbl.find Tables.label_table label)             
                    else interpret continue
        with Not_found -> 
            (
                print_string "Not found";
                exit 1
            )
    | _ -> (exit 1)

and interp_print (print_list : Absyn.printable list)
                 (continue : Absyn.program) =
    let print_item item =
        match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    in (List.iter print_item print_list; print_newline ());
    interpret continue


and interp_input (memref_list : Absyn.memref list)
                 (continue : Absyn.program)  =
    let input_number memref =
        try  let number = Etc.read_number ()
             in match memref with
             | Variable ident -> 
                    Hashtbl.add Tables.variable_table ident number
        with End_of_file -> 
             (
                 print_string "End_of_file"; 
                 (* once we read in EOF, we want to store in (eof, 1.0)
                    into variable_table
                 *)
                 Hashtbl.add Tables.variable_table "eof" 1.0;
                 print_newline ()
             )
    in List.iter input_number memref_list;
    interpret continue

and interp_STUB reason continue = (
    print_string "Unimplemented: ";
    print_string reason;
    print_newline();
    interpret continue)

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

