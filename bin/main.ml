open Yojson.Safe.Util

type log_type = Info | Error | Warning


let convert_lvl logs = to_string (member "level" logs)

let find_error lvl = match (convert_lvl lvl) with
| "ERROR" -> 1
| _ ->  0



let rec count_errors acc lst = match lst with 
| [] -> acc
| h::t -> count_errors (acc+(find_error h)) t


let file = "log.txt"



let rec read_all_lines acc cursor = match input_line cursor with
| exception End_of_file -> List.rev acc
| line -> read_all_lines (line::acc)  cursor

let () =
  (* Read file and display the first line *)
  let ic = open_in file in
  try

    let lst_of_logs = read_all_lines [] ic in
    let parsed_logs = List.map Yojson.Safe.from_string lst_of_logs in

    let error_count = count_errors 0 parsed_logs in
    print_endline (string_of_int (error_count));

    (* write the result to stdout *)
    flush stdout;
    (* write on the underlying device now *)
    close_in ic
    (* close the input channel *)
  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr ic;
    (* emergency closing *)
    raise e



