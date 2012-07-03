open Str;;
open BatString;;
open BatMap;;
open List;;
open Printf;;
open Ppl_ocaml;;

(* ---[ Types ]--- *)

exception Parse_error of string;;
exception Duplicate_var_error of string;;

type obj_dir_t = OD_Min | OD_Max;;

type cstr_sys = {
    mutable obj_dir : obj_dir_t;
    mutable vars_fwd : int StringMap.t;
    mutable vars_bkw : string IntMap.t;
    mutable vars_fwd_obj : Variable.t StringMap.t;
    mutable next_var_num : int;
    (* obj *)
    (* cstrs *)
};;

(* ---[ Helpers ]--- *)

(* Eg. "abc" =~ "^a" is true *)
let (=~) s re = Str.string_match (Str.regexp re) s 0;;

let print_str_list l = List.iter (fun x -> print_string x) l;;

let trim_split by line =
    let elems = Str.split (Str.regexp by) line in
    List.map (fun x -> BatString.trim x) elems;;

(* ---[ Variables ]--- *)
let add_var sys name =
    if StringMap.mem name sys.vars_fwd then
        raise (Duplicate_var_error name)
    else
        sys.vars_fwd <- StringMap.add name sys.next_var_num sys.vars_fwd;
        sys.vars_bkw <- IntMap.add sys.next_var_num name sys.vars_bkw;
        sys.next_var_num <- sys.next_var_num + 1;;

(* ---[ Parsing ]--- *)
let parse_min_line sys line = 
    let elems = trim_split  "," line in
    if List.length elems == 0 then raise (Parse_error("min: " ^ line));

    sys.obj_dir <- OD_Min;; (* XXX *)

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
    | "min"  -> parse_min_line sys (hd (List.tl elems)); ()
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
    next_var_num = 0;
};;

parse sys "test_input.opl";;
Printf.printf "Variables: %d\n" sys.next_var_num;;
