open Str;;
open BatString;;
open BatMap;;
open List;;
open Printf;;
open Ppl_ocaml;;

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

type var_val = VarVal of string * Gmp.Z.t;;

type cstr_sys = {
    mutable obj_dir : Ppl_ocaml.optimization_mode;
    mutable vars_fwd : int StringMap.t;
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
        ignore (Str.search_forward (Str.regexp "^[0-9]+$") s' 0);
        Gmp.Z.from_string s'
    with Not_found -> raise (Bad_int_error s);;

(* ---[ Variables ]--- *)
let add_var sys name =
    if StringMap.mem name sys.vars_fwd then
        raise (Duplicate_var_error name)
    else
        sys.vars_fwd <- StringMap.add name sys.next_var_num sys.vars_fwd;
        sys.vars_bkw <- IntMap.add sys.next_var_num name sys.vars_bkw;
        sys.vars_fwd_lx <- StringMap.add name (Variable sys.next_var_num) sys.vars_fwd_lx;
        sys.next_var_num <- sys.next_var_num + 1;;

let lookup_var_lx name sys = try
    StringMap.find name sys.vars_fwd_lx with
    | Not_found -> raise (Var_not_found_error name);;

let lookup_var_col_from_lx col sys = try
    IntMap.find col sys.vars_bkw with
    | Not_found -> raise (Col_not_found_error col);;


(* ---[ Constraints ]--- *)
let add_cstr sys lhs op rhs =
    let lx = match op with
    | ">=" -> Greater_Or_Equal(lhs, rhs)
    | "<=" -> Less_Or_Equal(lhs, rhs)
    | "==" -> Equal(lhs, rhs)
    | _    -> raise (Bad_oper_error op) in
    sys.cstrs <- List.append sys.cstrs [lx];;

(* ---[ Parsing ]--- *)
let parse_term (sys:cstr_sys) (s:string) : linear_expression =
    let elems = trim_split "*" s  in
    match elems with
    | x::[]    -> lookup_var_lx x sys
    | x::y::[] -> Times((parse_arb_int x), (lookup_var_lx y sys))
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

let parse_vars_line sys line =
    let elems = trim_split "," line in
    List.iter (add_var sys) elems; ();;

let parse_real_line sys line =
    let elems = trim_split ":" line in
    let prefix = (List.hd elems) in
    if List.length elems != 2 then raise (Parse_error line);
    match prefix with
    | "vars" -> parse_vars_line sys (List.nth elems 1)
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
    Printf.printf "%s=%s\n" name (Gmp.Z.to_string value);;

let print_results sys =
    List.iter (print_result) sys.result;;

let get_col_from_expression (lx:linear_expression) : int =
    match lx with
    | Variable(col) -> col
    | _ -> raise (Solver_error "unexpected non-Variable expression");;

let rec parse_result_expression sys (lx:linear_expression) = 
    match lx with
    | Plus (e1, e2) ->
            parse_result_expression sys e1;
            parse_result_expression sys e2;
    | Times (zval, col) ->
            let name = lookup_var_col_from_lx (get_col_from_expression  col) sys in
                sys.result <- List.append sys.result (VarVal(name, zval)::[])
    | _ -> 
            raise (Solver_error "Bad result expression from solver?");;

let get_result sys mip = 
    let pt = ppl_MIP_Problem_optimizing_point mip in
    match pt with
    | Point(lx, num) -> parse_result_expression sys lx
    | _ -> raise (Solver_error "solution was not a point");;

let solve (sys:cstr_sys) =
    let n_cstrs = List.length sys.cstrs in 
    let mip = ppl_new_MIP_Problem n_cstrs sys.cstrs sys.obj_fun sys.obj_dir in
    let status = ppl_MIP_Problem_solve mip in
    match status with
    | Optimized_Mip_Problem -> get_result sys mip
    | _                     -> raise (Solver_error "NOT OPTIMAL\n");;

(* ---[ MAIN ]--- *)
let sys = {
    obj_dir = Ppl_ocaml.Minimization;
    vars_fwd = StringMap.empty;
    vars_bkw = IntMap.empty;
    vars_fwd_lx = StringMap.empty;
    next_var_num = 0;
    obj_fun = Coefficient (Gmp.Z.of_int 0);
    cstrs = [];
    result = [];
};;

(* let test = Plus ((Variable 1), (Variable 2));; *)
parse sys "test_input.opl";;
Printf.printf "Variables: %d\n" sys.next_var_num;;
print_string("Objective Func: \n");;
Printf.printf "Constraints: %d\n" (List.length sys.cstrs);;
let res = solve sys;;
print_results sys;;
