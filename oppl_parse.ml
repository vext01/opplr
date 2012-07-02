open Str;;
open BatString;;
open List;;

(* Eg. "abc" =~ "^a" is true *)
let (=~) s re = Str.string_match (Str.regexp re) s 0;;

let parse_real_line line =
        let elems = Str.split (Str.regexp ":") line in
        let prefix = (List.hd elems) in
        match prefix with
        "min" -> print_string("MIN LINE\n"); ()
        | _ -> print_string("CONSTRAINT\n"); ();;

let parse_line line = match line with
        "" -> ()
        | _ -> (
                if BatString.starts_with line "#" then ()
                else parse_real_line line; ()
               );;

let parse filename = 
        let file = open_in filename in try
                while true do (parse_line (input_line file)) done
        with End_of_file -> close_in file; ();;

parse "test_input.opl";;
