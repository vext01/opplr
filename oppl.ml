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
type var_type = IntegerVar | RationalVar;;

type cstr_sys = {
    mutable obj_dir : Ppl_ocaml.optimization_mode;
    mutable vars_fwd : int StringMap.t;
    mutable vars_fwd_types : var_type StringMap.t;
    mutable vars_bkw : string IntMap.t;
    mutable vars_fwd_lx : linear_expression StringMap.t;
    mutable next_var_num : int;
    mutable obj_fun : linear_expression;
    mutable cstrs : linear_constraint list;
    mutable result : var_val list; 
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

let print_str_list l = List.iter (fun x -> print_string x) l;;

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
    if StringMap.mem name sys.vars_fwd then
        raise (Duplicate_var_error name)
    else
        sys.vars_fwd <- StringMap.add name sys.next_var_num sys.vars_fwd;
        sys.vars_bkw <- IntMap.add sys.next_var_num name sys.vars_bkw;
        sys.vars_fwd_lx <- StringMap.add name (Variable sys.next_var_num) sys.vars_fwd_lx;
        sys.next_var_num <- sys.next_var_num + 1;;

let lookup_var_lx sys name = try
    StringMap.find name sys.vars_fwd_lx with
    | Not_found -> raise (Var_not_found_error name);;

let lookup_var_col_from_lx sys col = try
    IntMap.find col sys.vars_bkw with
    | Not_found -> raise (Col_not_found_error col);;

let lookup_var_col_from_name sys name = try
    StringMap.find name sys.vars_fwd with
    | Not_found -> raise (Var_not_found_error name);;

(*
let type_vars sys vnames vtype = 
    let cols = List.map (lookup_var_col_from_name 
    *)

(* ---[ Constraints ]--- *)
let add_cstr sys lhs op rhs =
    let lc = match op with
    | ">=" -> Greater_Or_Equal(lhs, rhs)
    | "<=" -> Less_Or_Equal(lhs, rhs)
    | "==" -> Equal(lhs, rhs)
    | _    -> raise (Bad_oper_error op) in
    print_string "New linear constraint: ";
    print_linear_constraint lc;
    sys.cstrs <- List.append sys.cstrs [lc];;

(* ---[ Parsing ]--- *)
let parse_term (sys:cstr_sys) (s:string) : linear_expression =
    let elems = trim_split "*" s  in
    match elems with
    | x::[]    -> lookup_var_lx sys x
    | x::y::[] -> Times((parse_arb_int x), (lookup_var_lx sys y))
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
    print_string "\nAdding Objective Function: ";
    print_linear_expression obj_fun;
    print_string "\n";
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
    (*type_vars sys vtype elems;; *)

let parse_real_line sys line =
    let elems = trim_split ":" line in
    let prefix = (List.hd elems) in
    if List.length elems != 2 then raise (Parse_error line);
    match prefix with
    | "vars" -> parse_vars_line sys RationalVar (List.nth elems 1)
    | "int_vars" -> parse_vars_line sys IntegerVar (List.nth elems 1) 
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

let print_result (VarVal(name, value)) : unit =
    Printf.printf "%s=%s\n" name (Gmp.Q.to_string value);;

let print_results sys =
    List.iter (print_result) sys.result;;

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
            let name = lookup_var_col_from_lx sys (get_col_from_expression  col) in
                sys.result <- List.append sys.result (VarVal(name, qval)::[])
    | _ -> 
            raise (Solver_error "Bad result expression from solver?");;

let get_result sys mip = 
    let pt = ppl_MIP_Problem_optimizing_point mip in
    match pt with
    | Point(lx, denom) ->
            print_string "\nResult Expression:\n";
            print_linear_expression lx;
            parse_result_expression sys lx denom;
            print_string "\nDenominator:";
            print_string (Gmp.Z.to_string denom);
            print_string "\n"
    | _ -> raise (Solver_error "solution was not a point");;

let solve (sys:cstr_sys) =
    let n_cstrs = List.length sys.cstrs in 
    let mip = ppl_new_MIP_Problem n_cstrs sys.cstrs sys.obj_fun sys.obj_dir in
    let status = ppl_MIP_Problem_solve mip in
    match status with
    | Optimized_Mip_Problem -> get_result sys mip
    | _                     -> raise (Solver_error "NOT OPTIMAL");;

(* ---[ MAIN ]--- *)

let get_filename = 
    try
        Array.get Sys.argv 1
    with Invalid_argument(x) -> raise (Usage_error "usage: oppl <filename>");;

let sys = {
    obj_dir = Ppl_ocaml.Minimization;
    vars_fwd = StringMap.empty;
    vars_fwd_types = StringMap.empty;
    vars_bkw = IntMap.empty;
    vars_fwd_lx = StringMap.empty;
    next_var_num = 0;
    obj_fun = Coefficient Gmp.Z.zero;
    cstrs = [];
    result = [];
};;

let oppl_version = "0.1";;
let oppl_years = "2012";;
Printf.printf "\nOPPL Version %s\n(C) Edd Barrett %s\n" oppl_version oppl_years;;
print_string "-----------------------------------------\n";;

parse sys get_filename;;
print_string "Constraint system loaded:\n";;
Printf.printf "Variables: %d\n" sys.next_var_num;;
print_string("Objective Func: \n");;
Printf.printf "Constraints: %d\n" (List.length sys.cstrs);;
let res = solve sys;;
print_results sys;;
