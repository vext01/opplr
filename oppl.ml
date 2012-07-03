open Str;;
open BatString;;
open BatMap;;
open List;;
open Printf;;
open Ppl_ocaml;;

(* ---[ Types ]--- *)

exception Parse_error of string;;
exception Duplicate_var_error of string;;
exception Empty_list_error;;
exception Bad_term_error of string;;

type obj_dir_t = OD_Min | OD_Max;;

type cstr_sys = {
    mutable obj_dir : obj_dir_t;
    mutable vars_fwd : int StringMap.t;
    mutable vars_bkw : string IntMap.t;
    mutable vars_fwd_lx : linear_expression StringMap.t;
    mutable next_var_num : int;
    mutable obj_fun : linear_expression;
    (* cstrs *)
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

(* ---[ Variables ]--- *)
let add_var sys name =
    if StringMap.mem name sys.vars_fwd then
        raise (Duplicate_var_error name)
    else
        sys.vars_fwd <- StringMap.add name sys.next_var_num sys.vars_fwd;
        sys.vars_bkw <- IntMap.add sys.next_var_num name sys.vars_bkw;
        sys.vars_fwd_lx <- StringMap.add name (Variable sys.next_var_num) sys.vars_fwd_lx;
        sys.next_var_num <- sys.next_var_num + 1;;

(* ---[ Parsing ]--- *)

let parse_term (sys:cstr_sys) (s:string) : linear_expression =
    let elems = trim_split "*" s  in
    match List.length elems with
    | 1 -> StringMap.find (List.hd elems) sys.vars_fwd_lx
    | 2 -> Times(
            (Gmp.Z.from_string (List.nth elems 0)),
            (StringMap.find (List.nth elems 1) sys.vars_fwd_lx)
           )
    | _ -> raise (Bad_term_error s);;

let parse_obj_line sys dir line =
    let elems = trim_split  "," line in
    if List.length elems == 0 then raise (Parse_error("min: " ^ line));
    (*let vars = List.map (fun x -> StringMap.find x sys.vars_fwd_lx) elems in *)
    let vars = List.map (parse_term sys) elems in
    let obj_fun = fold1_left (fun x y -> Plus(x, y)) vars in
    sys.obj_fun <- obj_fun;
    sys.obj_dir <- dir;;

let parse_cstr_line sys line = ();;

let parse_vars_line sys line =
    let elems = trim_split "," line in
    List.iter (add_var sys) elems; ();;

let parse_real_line sys line =
    let elems = trim_split ":" line in
    let prefix = (List.hd elems) in
    if List.length elems != 2 then raise (Parse_error line);
    match prefix with
    | "vars" -> parse_vars_line sys (hd (List.tl elems)); ()
    | "min"  -> parse_obj_line sys OD_Min (hd (List.tl elems)); ()
    | _      -> parse_cstr_line sys line; ();;

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

(* ---[ MAIN ]--- *)
let sys = {
    obj_dir = OD_Min;
    vars_fwd = StringMap.empty;
    vars_bkw = IntMap.empty;
    vars_fwd_lx = StringMap.empty;
    next_var_num = 0;
    obj_fun = Coefficient (Gmp.Z.of_int 0);
};;

(* let test = Plus ((Variable 1), (Variable 2));; *)
parse sys "test_input.opl";;
Printf.printf "Variables: %d\n" sys.next_var_num;;
print_string("Objective Func: \n");;
print_linear_expression sys.obj_fun;;
print_string("\n");;
