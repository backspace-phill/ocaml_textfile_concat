let usage_msg = "append [-verbose] <file1> [<file2>] ... -o <output>"
let verbose = ref false
let input_files = ref []
let output_file = ref ""
let file_append filename =
  input_files := filename :: !input_files
let speclist =
  [("-verbose", Arg.Set verbose, "Output debug information");
   ("-o", Arg.Set_string output_file, "Set output file name")]
let exit_on_no_output output_file =
  match output_file with
  | "" -> true
  | _ -> false
let exit_on_no_input input_files =
  match input_files with
  | [] -> true
  | _ -> false
let read_text_of_file file = 
  let f = open_in_bin file in
  let s = really_input_string f (in_channel_length f) in
  close_in_noerr f;
  s
let rec stringlist_to_string input_list = 
  match input_list with
  | [x] -> x
  | [] -> ""
  | h::t -> stringlist_to_string t ^ h
let read_text_from_files input =
  List.map read_text_of_file input |> stringlist_to_string
let output_text_to_file file_name file_data = 
  let oc = open_out file_name in
  Printf.fprintf oc "%s" file_data;
  close_out_noerr oc
let () =
  Arg.parse speclist file_append usage_msg;
  if exit_on_no_output !output_file then (print_endline usage_msg; exit (-1)); 
  if exit_on_no_input !input_files then (print_endline usage_msg; exit (-1)); 
  read_text_from_files !input_files |> output_text_to_file !output_file