open Yojson.Safe.Util
type log_type = Info | Error | Warning

let line =
  {|{"level":"ERROR","service":"payments","message":"Database connection failed"}|}

  let line2 =
  {|{"level":"ERROR","service":"payments","message":"Database connection failed"}|}

 
  let line3 =
  {|{"level":"ERROR","service":"payments","message":"Database connection failed"}|}


let lst_of_logs = [line;line2;line3]

let parsed_logs =
  List.map Yojson.Safe.from_string lst_of_logs;;

let convert_lvl logs = to_string (member "level" logs)

let find_error lvl = match (convert_lvl lvl) with
| "ERROR" -> 1
| _ ->  0



let rec count_errors acc lst = match lst with 
| [] -> acc
| h::t -> count_errors (acc+(find_error h)) t


let file = "log.txt"

let () =
  (* Read file and display the first line *)
  let ic = open_in file in
  try
    let line = input_line ic in
    (* read line, discard \n *)
    print_endline line;
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



