module ErrorKey = struct
  type t = string * string
  let compare = Stdlib.compare
end

module ErrorMap = Map.Make(ErrorKey)



let counts : int ErrorMap.t = ErrorMap.empty

let count_error counts service message =
  let key = (service, message) in
  let next_count =
    match ErrorMap.find_opt key counts with
    | None -> 1
    | Some n -> n + 1
  in
  ErrorMap.add key next_count counts


let file = "log.txt"


let update_counts counts line =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string line in
  match json |> member "level" |> to_string with
  | "ERROR" ->
      let service = json |> member "service" |> to_string in
      let message = json |> member "message" |> to_string in
      count_error counts service message
  | _ -> counts

let rec read_all_lines counts cursor = 
  match input_line cursor with
| exception End_of_file -> counts
| line -> read_all_lines (update_counts counts line)  cursor

let () =
  let counts = In_channel.with_open_text file (fun ic ->
    read_all_lines ErrorMap.empty ic)
  in
  let rows =
    ErrorMap.bindings counts
    |> List.sort (fun (_, a) (_, b) -> Int.compare b a)
  in
  let total = List.fold_left (fun sum (_, n) -> sum + n) 0 rows in
  Printf.printf "%d errors\n" total;
  List.iter
    (fun ((service, message), n) ->
      Printf.printf "%4d  %s: %s\n" n service message)
    rows



