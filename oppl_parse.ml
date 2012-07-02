open Str;;
open BatString;;

(* Eg. "abc" =~ "^a" is true *)
let (=~) s re = Str.string_match (Str.regexp re) s 0;;

(*
let parse_line line =
        print_string ("GOT: '" ^ line ^ "'\n");
        (
        if BatString.is_empty line then
                ()
        else
                print_string "not empty\n";

        );;
*)

let parse_line line =
        print_string ("GOT: '" ^ line ^ "'\n");
        match line with
        "" -> print_string "EMPTY\n"; ()
        | _ -> (if BatString.starts_with line "#" then ()
                else print_string "REAL_LINE\n"; ()
               );;
let parse filename = 
        let file = open_in filename in try
                while true do (parse_line (input_line file)) done
        with End_of_file -> close_in file; ();;

parse "test_input.opl";;
