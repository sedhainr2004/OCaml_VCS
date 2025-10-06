open Unix
open Printf
open FileUtil

type file_info = {
  filename : string;
  hash : string;
}

type record = {
  id : string;
  message : string;
  date : string;
  all_files : file_info list;
  changed_files : file_info list;
}

let records_file = "records.txt"
let data_dir = "data"
let tracked_files_file = "tracked_files.txt"

(* Helper function to read records from records.txt *)
let read_records () =
  if Sys.file_exists records_file then
    let ic = open_in records_file in
    let rec loop acc =
      try
        let line = input_line ic in
        let parts = String.split_on_char ',' line in
        match parts with
        | [ id; message; date; all_files_str; changed_files_str ] ->
            let parse_files str =
              String.split_on_char ';' str
              |> List.filter_map (fun file_info ->
                     match String.split_on_char ':' file_info with
                     | [ filename; hash ] -> Some { filename; hash }
                     | _ ->
                         Printf.printf "Warning: Invalid file info: %s\n"
                           file_info;
                         None)
            in
            let all_files = parse_files all_files_str in
            let changed_files = parse_files changed_files_str in
            let record = { id; message; date; all_files; changed_files } in
            loop (record :: acc)
        | _ ->
            Printf.printf "Warning: Invalid record format: %s\n" line;
            loop acc
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    loop []
  else []

(* Helper function to write a record to records.txt *)
let write_record record =
  let files_to_string files =
    files
    |> List.map (fun file_info ->
           Printf.sprintf "%s:%s" file_info.filename file_info.hash)
    |> String.concat ";"
  in
  let all_files_str = files_to_string record.all_files in
  let changed_files_str = files_to_string record.changed_files in
  let record_string =
    Printf.sprintf "%s,%s,%s,%s,%s\n" record.id record.message record.date
      all_files_str changed_files_str
  in
  let oc = open_out_gen [ Open_append; Open_creat ] 0o644 records_file in
  output_string oc record_string;
  close_out oc

let node_id = Random.int 1024 (* Unique identifier for this node *)
let epoch = 1609459200.0 (* Custom epoch (e.g., 2021-01-01 00:00:00 UTC) *)
let sequence = ref 0
let last_timestamp = ref 0.0

(* Function to generate a unique ID *)
let generate_id () =
  let current_time = Unix.gettimeofday () in
  let timestamp = int_of_float ((current_time -. epoch) *. 1000.0) in
  if timestamp = int_of_float !last_timestamp then
    sequence := (!sequence + 1) land 4095 (* 12 bits for sequence *)
  else sequence := 0;
  last_timestamp := float_of_int timestamp;
  if !sequence = 0 && timestamp <= int_of_float !last_timestamp then
    Unix.sleepf 0.001;
  (* Wait for the next millisecond *)
  let id =
    Int64.(
      logor
        (shift_left (of_int timestamp) 22)
        (logor (shift_left (of_int node_id) 12) (of_int !sequence)))
  in
  Printf.sprintf "%Lx" id (* Changed to print hex representation *)

(* Function to get all tracked files *)
let get_all_tracked_files () =
  if Sys.file_exists tracked_files_file then (
    let ic = open_in tracked_files_file in
    let result =
      try
        let content = input_line ic in
        if String.length content = 0 then []
        else String.split_on_char ';' content
      with End_of_file -> []
    in
    close_in ic;
    result)
  else []

(* Helper function to update tracked files *)
let update_tracked_files new_files =
  let current_files = get_all_tracked_files () in
  let updated_files =
    List.sort_uniq String.compare (current_files @ new_files)
  in
  let oc = open_out tracked_files_file in
  output_string oc (String.concat ";" updated_files);
  close_out oc

(* The 'give' command implementation *)
let give filenames message =
  Printf.printf "Debug: data_dir = %s\n" data_dir;
  if not (Sys.file_exists data_dir) then (
    Printf.printf "Debug: Creating data_dir\n";
    Unix.mkdir data_dir 0o755);
  let id = generate_id () in
  Printf.printf "Debug: Generated ID = %s\n" id;
  let time = Unix.localtime (Unix.time ()) in
  let date =
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (time.tm_year + 1900)
      (time.tm_mon + 1) time.tm_mday time.tm_hour time.tm_min time.tm_sec
  in
  let version_dir = Filename.concat data_dir id in
  Printf.printf "Debug: version_dir = %s\n" version_dir;
  Unix.mkdir version_dir 0o755;

  (* Update tracked files *)
  update_tracked_files filenames;

  (* Get all tracked files *)
  let all_tracked_files = get_all_tracked_files () in

  let all_files = ref [] in
  let changed_files = ref [] in

  List.iter
    (fun filename ->
      if Sys.file_exists filename then (
        let dest = Filename.concat version_dir filename in
        FileUtil.cp [ filename ] dest;
        let hash = Digest.file filename |> Digest.to_hex in
        let file_info = { filename; hash } in
        all_files := file_info :: !all_files;
        if List.mem filename filenames then
          changed_files := file_info :: !changed_files
        else Printf.printf "Warning: File %s was not changed.\n" filename))
    all_tracked_files;

  if List.length !all_files > 0 then (
    let record =
      {
        id;
        message;
        date;
        all_files = !all_files;
        changed_files = !changed_files;
      }
    in
    write_record record;
    print_endline ("Saved version with ID: " ^ id))
  else print_endline "Error: No valid files to commit."

(* The 'get' command implementation *)
let get id =
  let records = read_records () in
  try
    let record = List.find (fun r -> r.id = id) records in
    List.iter
      (fun file_info ->
        let source =
          Filename.concat (Filename.concat data_dir id) file_info.filename
        in
        if Sys.file_exists source then FileUtil.cp [ source ] file_info.filename
        else
          Printf.printf "Warning: File %s not found in version %s\n"
            file_info.filename id)
      record.all_files;
    print_endline ("Restored version with ID: " ^ id)
  with Not_found -> print_endline "ID not found in records."

let format_changed_files changed_files =
  List.map (fun f -> f.filename) changed_files

(* Helper function to format the records into a string representation *)
let generate_show () =
  let records = read_records () in
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer
    (Printf.sprintf "%-20s %-20s %-25s %s\n" "ID" "Date" "Changed Files"
       "Message");
  List.iter
    (fun r ->
      let changed_files = format_changed_files r.changed_files in
      match changed_files with
      | [] ->
          Buffer.add_string buffer
            (Printf.sprintf "%-20s %-20s %-25s %s\n" r.id r.date "" r.message)
      | [ file ] ->
          Buffer.add_string buffer
            (Printf.sprintf "%-20s %-20s %-25s %s\n" r.id r.date file r.message)
      | file :: rest ->
          Buffer.add_string buffer
            (Printf.sprintf "%-20s %-20s %-25s %s\n" r.id r.date file r.message);
          List.iter
            (fun f ->
              Buffer.add_string buffer
                (Printf.sprintf "%-20s %-20s %-25s\n" "" "" f))
            rest)
    records;
  Buffer.contents buffer

(* Printer function that takes a string and prints it *)
let show_printer output_str = print_string output_str

(* Main show function *)
let show () = generate_show () |> show_printer

(* The 'del' command implementation *)
let del id =
  let records = read_records () in
  if List.exists (fun r -> r.id = id) records then
    let version_dir = Filename.concat data_dir id in
    if Sys.file_exists version_dir then (
      FileUtil.rm ~recurse:true [ version_dir ];
      let updated_records = List.filter (fun r -> r.id <> id) records in
      let oc = open_out records_file in
      List.iter
        (fun r ->
          let all_files_str =
            String.concat ";"
              (List.map (fun f -> f.filename ^ ":" ^ f.hash) r.all_files)
          in
          let changed_files_str =
            String.concat ";"
              (List.map (fun f -> f.filename ^ ":" ^ f.hash) r.changed_files)
          in
          Printf.fprintf oc "%s,%s,%s,%s,%s\n" r.id r.message r.date
            all_files_str changed_files_str)
        updated_records;
      close_out oc;
      print_endline ("Deleted version with ID: " ^ id))
    else print_endline "Version directory not found."
  else print_endline "ID not found in records."

(* Helper function to read file contents *)
let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  String.split_on_char '\n' s

(* LCS algorithm implementation *)
let lcs a b =
  let m = Array.length a and n = Array.length b in
  let c = Array.make_matrix (m + 1) (n + 1) 0 in
  for i = 1 to m do
    for j = 1 to n do
      c.(i).(j) <-
        (if a.(i - 1) = b.(j - 1) then c.(i - 1).(j - 1) + 1
         else max c.(i - 1).(j) c.(i).(j - 1))
    done
  done;
  c

(* Backtrack to find the diff *)
let backtrack c a b =
  let rec loop i j acc =
    if i > 0 && j > 0 && a.(i - 1) = b.(j - 1) then
      loop (i - 1) (j - 1) ((`Both, a.(i - 1)) :: acc)
    else if j > 0 && (i = 0 || c.(i).(j - 1) >= c.(i - 1).(j)) then
      loop i (j - 1) ((`Add, b.(j - 1)) :: acc)
    else if i > 0 && (j = 0 || c.(i).(j - 1) < c.(i - 1).(j)) then
      loop (i - 1) j ((`Remove, a.(i - 1)) :: acc)
    else acc
  in
  loop (Array.length a) (Array.length b) []

(* Helper function to generate a diff string between two versions of a file *)
let generate_diff id1 id2 filename =
  let records = read_records () in
  try
    (* Check if both IDs exist in the records *)
    if not (List.exists (fun r -> r.id = id1) records) then
      Printf.sprintf "Error: Version ID %s not found.\n" id1
    else if not (List.exists (fun r -> r.id = id2) records) then
      Printf.sprintf "Error: Version ID %s not found.\n" id2
    else
      let file1_path =
        Filename.concat (Filename.concat data_dir id1) filename
      in
      let file2_path =
        Filename.concat (Filename.concat data_dir id2) filename
      in
      if not (Sys.file_exists file1_path) then
        Printf.sprintf "Error: File %s does not exist in version %s\n" filename
          id1
      else if not (Sys.file_exists file2_path) then
        Printf.sprintf "Error: File %s does not exist in version %s\n" filename
          id2
      else
        let lines1 = Array.of_list (read_file file1_path) in
        let lines2 = Array.of_list (read_file file2_path) in
        let c = lcs lines1 lines2 in
        let diff_result = backtrack c lines1 lines2 in
        let buffer = Buffer.create 1024 in
        Buffer.add_string buffer
          (Printf.sprintf "Diff between %s (version %s) and %s (version %s):\n"
             filename id1 filename id2);
        List.iter
          (function
            | `Both, line ->
                Buffer.add_string buffer (Printf.sprintf "  %s\n" line)
            | `Add, line ->
                Buffer.add_string buffer (Printf.sprintf "+ %s\n" line)
            | `Remove, line ->
                Buffer.add_string buffer (Printf.sprintf "- %s\n" line))
          diff_result;
        Buffer.contents buffer
  with Not_found ->
    Printf.sprintf "Error: One or both version IDs not found.\n"

(* Helper function to print the diff string *)
let diff_printer diff_str = print_string diff_str

(* Main diff function. Lines prefixed with '-' are present in the first version
   but not the second. Lines prefixed with '+' are present in the second version
   but not the first. Lines prefixed with ' ' are present in both versions.*)
let diff id1 id2 filename = generate_diff id1 id2 filename |> diff_printer

(* New function to check for changes *)
let generate_changes () =
  let tracked_files = get_all_tracked_files () in
  let records = read_records () in
  match records with
  | latest_record :: _ ->
      let changed_files = ref [] in
      List.iter
        (fun filename ->
          let current_file_path = filename in
          let latest_version_path =
            Filename.concat (Filename.concat data_dir latest_record.id) filename
          in
          if
            Sys.file_exists current_file_path
            && Sys.file_exists latest_version_path
          then
            let lines1 = Array.of_list (read_file current_file_path) in
            let lines2 = Array.of_list (read_file latest_version_path) in
            let c = lcs lines1 lines2 in
            let diff_result = backtrack c lines1 lines2 in
            if
              List.exists
                (function
                  | `Add, _ | `Remove, _ -> true
                  | _ -> false)
                diff_result
            then changed_files := filename :: !changed_files)
        tracked_files;
      if !changed_files = [] then "No changes detected in tracked files."
      else
        let header = "Changes detected in the following files:\n" in
        let file_list =
          List.map
            (fun filename -> Printf.sprintf "- %s" filename)
            !changed_files
        in
        header ^ String.concat "\n" file_list
  | [] -> "No versions found. Unable to check for changes."

let changes_printer changes_str = print_endline changes_str
let changes () = generate_changes () |> changes_printer
