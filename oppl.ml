(*
 * Copyright (c) 2012), Edd Barrett <vext01@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

open Str;;
open BatString;;
open BatMap;;
open List;;
open Printf;;
open Ppl_ocaml;;
open Sys;;
open BatList;;
open Batteries_uni;;

(* ---[ Types ]--- *)

exception Parse_error of string;;
exception Duplicate_var_error of string;;
exception Var_not_found_error of string;;
exception Col_not_found_error of int;;
exception Empty_list_error;;
exception Bad_term_error of string;;
exception Bad_int_error of string;;
exception Bad_oper_error of string;;
exception Solver_error of string;;
exception Usage_error of string;;

type var_val = VarVal of string * Gmp.Q.t;;
type var_type = IntegerVar | RationalVar | BinaryVar;;

type cstr_sys = {
    (* variable mappings *)
    mutable map_varname_to_col : int StringMap.t;
    mutable map_varname_to_type : var_type StringMap.t;
    mutable map_varcol_to_name : string IntMap.t;
    mutable map_varname_to_lexpr : linear_expression StringMap.t;
    (* MILP problem description *)
    mutable obj_fun : linear_expression;
    mutable obj_dir : Ppl_ocaml.optimization_mode;
    mutable cstrs : linear_constraint list;
    mutable result_map : Gmp.Q.t StringMap.t;
    (* Misc *)
    mutable next_var_num : int;
};;

(* ---[ Helpers ]--- *)

let rec print_linear_expression = function
    Variable v ->
        print_string "V(";
        print_int v;
        print_string ")";
  | Coefficient c ->
          print_string(Gmp.Z.to_string c)
  | Unary_Minus e ->
          print_string "-(";
          print_linear_expression e;
          print_string ")";
  | Unary_Plus e ->
          print_linear_expression e
  | Plus (e1, e2) ->
          print_string "(";
          print_linear_expression e1;
          print_string " + ";
          print_linear_expression e2;
          print_string ")";
  | Minus (e1, e2) ->
          print_string "(";
          print_linear_expression e1;
          print_string " - ";
          print_linear_expression e2;
          print_string ")";
  | Times (c, e) ->
          print_string(Gmp.Z.to_string c);
          print_string "*(";
          print_linear_expression e;
          print_string ")";
;;

let print_linear_constraint lc =
    match lc with
    | Equal(lhs, rhs) ->
        print_linear_expression lhs;
        print_string "=";
        print_linear_expression rhs;
        print_string "\n"
    | Less_Or_Equal(lhs, rhs) ->
        print_linear_expression lhs;
        print_string "<=";
        print_linear_expression rhs;
        print_string "\n"
    | Greater_Or_Equal(lhs, rhs) ->
        print_linear_expression lhs;
        print_string ">=";
        print_linear_expression rhs;
        print_string "\n"
    | _ -> raise (Bad_oper_error "unknown relation");;

(* Eg. "abc" =~ "^a" is true *)
let (=~) s re = Str.string_match (Str.regexp re) s 0;;

let print_str_list l = List.iter (fun x -> print_string x) l; print_string "\n";;
let print_int_list l = List.iter (fun x -> print_int x) l; print_string "\n";;

let trim_split by line =
    let elems = Str.split (Str.regexp by) line in
    List.map (fun x -> BatString.trim x) elems;;

let fold1_left f l = 
    match l with
    | x :: [] -> x
    | x :: xs -> fold_left f x xs
    | []      -> raise Empty_list_error;;

(* checked big integer parsing from a string *)
let parse_arb_int s =
    let s' = BatString.trim s in 
    try
        ignore (Str.search_forward (Str.regexp "^-?[0-9]+$") s' 0);
        Gmp.Z.from_string s'
    with Not_found -> raise (Bad_int_error s);;

(* ---[ Variables ]--- *)
let add_var sys vtype name =
    if StringMap.mem name sys.map_varname_to_col then
        raise (Duplicate_var_error name)
    else
        sys.map_varname_to_col <- StringMap.add name sys.next_var_num sys.map_varname_to_col;
        sys.map_varcol_to_name <- IntMap.add sys.next_var_num name sys.map_varcol_to_name;
        sys.map_varname_to_lexpr <- StringMap.add name (Variable sys.next_var_num) sys.map_varname_to_lexpr;
        sys.next_var_num <- sys.next_var_num + 1;
        sys.map_varname_to_type <- StringMap.add name vtype sys.map_varname_to_type;;

let lookup_lexpr_from_varname sys name = try
    StringMap.find name sys.map_varname_to_lexpr with
    | Not_found -> raise (Var_not_found_error name);;

let lookup_var_col_from_lexpr sys col = try
    IntMap.find col sys.map_varcol_to_name with
    | Not_found -> raise (Col_not_found_error col);;

let lookup_var_col_from_name sys name = try
    StringMap.find name sys.map_varname_to_col with
    | Not_found -> raise (Var_not_found_error name);;

let is_z_var ty =
    match ty with
    | IntegerVar -> true
    | _ -> false;;

let is_q_var ty =
    match ty with
    | RationalVar -> true
    | _ -> false;;

let is_b_var ty =
    match ty with
    | BinaryVar -> true
    | _ -> false;;

(* Add 0 <= b <= 1 for a binary variable b *)
let constrain_binary_var sys mip name = 
    let lhs = lookup_lexpr_from_varname sys name in
    let lobo = Greater_Or_Equal(lhs, Coefficient Gmp.Z.zero) in
    let upbo = Less_Or_Equal(lhs, Coefficient Gmp.Z.one) in
    ppl_MIP_Problem_add_constraint mip lobo;
    ppl_MIP_Problem_add_constraint mip upbo;;

let type_vars sys mip =
    let z_vars = StringMap.filter is_z_var sys.map_varname_to_type in
    let b_vars = StringMap.filter is_b_var sys.map_varname_to_type in
    let z_cols = [? lookup_var_col_from_name sys x | x <- (StringMap.keys z_vars) ?] in 
    let b_cols = [? lookup_var_col_from_name sys x | x <- (StringMap.keys b_vars) ?] in
    Ppl_ocaml.ppl_MIP_Problem_add_to_integer_space_dimensions mip (List.of_enum z_cols);
    Ppl_ocaml.ppl_MIP_Problem_add_to_integer_space_dimensions mip (List.of_enum b_cols);
    List.iter (constrain_binary_var sys mip) (List.of_enum (StringMap.keys b_vars));;

let number_of_bin_vars sys =
    let z_vars = StringMap.filter is_z_var sys.map_varname_to_type in
    let keys = StringMap.keys z_vars in
    List.length (List.of_enum keys);;

(* ---[ Constraints ]--- *)
let add_cstr sys lhs op rhs =
    let lc = match op with
    | ">=" -> Greater_Or_Equal(lhs, rhs)
    | "<=" -> Less_Or_Equal(lhs, rhs)
    | "==" -> Equal(lhs, rhs)
    | _    -> raise (Bad_oper_error op) in
    sys.cstrs <- List.append sys.cstrs [lc];;

(* ---[ Parsing ]--- *)
let parse_term (sys:cstr_sys) (s:string) : linear_expression =
    let elems = trim_split "*" s  in
    match elems with
    | x::[]    -> lookup_lexpr_from_varname sys x
    | x::y::[] -> Times((parse_arb_int x), (lookup_lexpr_from_varname sys y))
    | _        -> raise (Bad_term_error s);;

let sum_terms terms = 
    fold1_left (fun x y -> Plus(x, y)) terms;;

let parse_terms sys line =
    let elems = trim_split  "," line in
    match elems with
    | [] -> raise (Parse_error line)
    | _  -> let terms = List.map (parse_term sys) elems in
            sum_terms terms;;

let parse_obj_line sys dir line =
    let obj_fun = parse_terms sys line in
    sys.obj_fun <- obj_fun;
    sys.obj_dir <- dir;;

let parse_cstr_line sys cstr_name line =
    let re = "^\\([^>=<]*\\)\\(<=\\|==\\|>=\\)\\([^>=<]*\\)$" in 
    let ok = Str.string_match (Str.regexp re) line 0 in
    match ok with
    | true -> let lhs = (Str.matched_group 1 line) in
              let op = (Str.matched_group 2 line) in
              let rhs = (Str.matched_group 3 line) in
              add_cstr sys (parse_terms sys lhs) op (Coefficient (parse_arb_int rhs))
    | _    -> raise (Parse_error line);;

let parse_vars_line sys vtype line =
    let elems = trim_split "," line in
    List.iter (add_var sys vtype) elems; ();;

let parse_real_line sys line =
    let elems = trim_split ":" line in
    let prefix = (List.hd elems) in
    if List.length elems != 2 then raise (Parse_error line);
    match prefix with
    | "vars" -> parse_vars_line sys RationalVar (List.nth elems 1)
    | "int_vars" -> parse_vars_line sys IntegerVar (List.nth elems 1) 
    | "bin_vars" -> parse_vars_line sys BinaryVar (List.nth elems 1) 
    | "min"  -> parse_obj_line sys Ppl_ocaml.Minimization (List.nth elems 1)
    | "max"  -> parse_obj_line sys Ppl_ocaml.Maximization (List.nth elems 1)
    | _      -> parse_cstr_line sys prefix (List.nth elems 1);;

let parse_line sys line =
    match line with
    | "" -> ()
    | _  -> (
        if BatString.starts_with line "#" then ()
        else parse_real_line sys line; ()
        );;

let parse sys filename = 
    let file = open_in filename in try
        while true do (parse_line sys (input_line file)) done
        with End_of_file -> close_in file; ();;

(* ---[ Solving ]--- *)

let write_result outfile vname qval =
    Printf.fprintf outfile "%s=%s\n" vname (Gmp.Q.to_string qval);;

let write_results sys out_filename =
    let outfile = open_out out_filename in
    StringMap.iter (write_result outfile) sys.result_map;
    close_out outfile;;

let get_col_from_expression (lx:linear_expression) : int =
    match lx with
    | Variable(col) -> col
    | _ -> raise (Solver_error "unexpected non-Variable expression");;

let rec parse_result_expression sys (lx:linear_expression) denom = 
    match lx with
    | Plus (e1, e2) ->
            parse_result_expression sys e1 denom;
            parse_result_expression sys e2 denom;
    | Times (zval, col) ->
            let qval =  Gmp.Q.from_zs zval denom in
            let name = lookup_var_col_from_lexpr sys (get_col_from_expression col) in
                sys.result_map <- StringMap.add name qval sys.result_map
    | _ -> 
            raise (Solver_error "Bad result expression from solver?");;

let init_result_map sys =
    let curry = fun sys vname ->
        sys.result_map <- (StringMap.add vname Gmp.Q.zero sys.result_map) in
    List.iter (curry sys) (List.of_enum (StringMap.keys sys.map_varname_to_col));;

let get_result sys mip = 
    (* first fill the result mapping wit zeroes values, this is
     * because a variable with a 0 coeff in the solution will not
     * appear in the result expression
     *)
    init_result_map sys;
    let pt = ppl_MIP_Problem_optimizing_point mip in
    match pt with
    | Point(lx, denom) -> parse_result_expression sys lx denom;
    | _ -> raise (Solver_error "solution was not a point");;

let solve (sys:cstr_sys) =
    let n_cstrs = List.length sys.cstrs + ((number_of_bin_vars sys) * 2) in
    (* because of two extra cstrs 0 <= b <= 1 for BinVar *)
    let mip = ppl_new_MIP_Problem n_cstrs sys.cstrs sys.obj_fun sys.obj_dir in
    ignore (type_vars sys mip);
    let status = ppl_MIP_Problem_solve mip in
    match status with
    | Optimized_Mip_Problem -> get_result sys mip
    | _                     -> raise (Solver_error "NOT OPTIMAL");;

(* ---[ MAIN ]--- *)

let get_filenames = 
    try
        (Array.get Sys.argv 1, Array.get Sys.argv 2)
    with Invalid_argument(x) -> raise (Usage_error "usage: oppl <filename> <outfile>");;

let sys = {
    obj_dir = Ppl_ocaml.Minimization;
    map_varname_to_col = StringMap.empty;
    map_varname_to_type = StringMap.empty;
    map_varcol_to_name = IntMap.empty;
    map_varname_to_lexpr = StringMap.empty;
    next_var_num = 0;
    obj_fun = Coefficient Gmp.Z.zero;
    cstrs = [];
    result_map = StringMap.empty;
};;

(* sometimes useful for debug *)
(*
parse sys get_filename;;
print_string "Constraint system loaded:\n";;
Printf.printf "Variables: %d\n" sys.next_var_num;;
print_string("Objective Func: \n");;
print_linear_expression sys.obj_fun;;
Printf.printf "\n\nConstraints: %d\n" (List.length sys.cstrs);;
let res = solve sys;;
Printf.printf "\nResult:\n";;
*)

let (infile, outfile) = get_filenames;;
parse sys infile;;
let res = solve sys;;
write_results sys outfile;;
