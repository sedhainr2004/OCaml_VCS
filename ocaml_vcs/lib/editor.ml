open Server
open Sys
open Str

let generate_summary record_id filename =
  let records = read_records () in
  let record_opt = List.find_opt (fun r -> r.id = record_id) records in
  match record_opt with
  | None -> Printf.sprintf "Error: Record with ID %s not found.\n" record_id
  | Some record ->
      let file_exists =
        List.exists (fun fi -> fi.filename = filename) record.all_files
      in

      if not file_exists then
        Printf.sprintf "Error: File %s not found in record %s.\n" filename
          record_id
      else
        let file_path =
          Filename.concat (Filename.concat "data" record_id) filename
        in
        let lines = read_file file_path in

        let line_count = List.length lines in
        let word_count =
          List.fold_left
            (fun acc line ->
              acc
              + (String.split_on_char ' ' line
                |> List.filter (fun w -> w <> "")
                |> List.length))
            0 lines
        in
        let char_count =
          List.fold_left (fun acc line -> acc + String.length line) 0 lines
        in

        let dash_line = String.make 40 '-' in
        Printf.sprintf
          "%s\n\
           | File Summary: %-22s |\n\
           | Record ID: %-25s |\n\
           %s\n\
           | Line count:      %-19d |\n\
           | Word count:      %-19d |\n\
           | Character count: %-19d |\n\
           %s\n"
          dash_line filename record_id dash_line line_count word_count
          char_count dash_line

let file_summary_printer summary_str = print_string summary_str

let file_summary record_id filename =
  generate_summary record_id filename |> file_summary_printer

let generate_n_longest_words record_id n_str =
  let parse_n n_str =
    match int_of_string_opt n_str with
    | Some n when n > 0 -> Ok n
    | Some _ -> Error "Error: The value of 'n' must be a positive integer.\n"
    | None ->
        Error "Error: Invalid value for 'n'. Please provide a valid integer.\n"
  in

  match parse_n n_str with
  | Error err -> err
  | Ok n -> (
      (* Attempt to find the record *)
      let record_opt =
        try
          Some
            (Ocamlvcs__Server.read_records ()
            |> List.find (fun r -> r.id = record_id))
        with Not_found -> None
      in
      match record_opt with
      | None -> Printf.sprintf "Error: Record with ID %s not found.\n" record_id
      | Some record -> (
          (* Attempt to read all files and gather words *)
          try
            let all_words =
              List.fold_left
                (fun acc file_info ->
                  let file_path =
                    Printf.sprintf "data/%s/%s" record_id file_info.filename
                  in
                  let lines = Server.read_file file_path in
                  let words =
                    List.flatten
                      (List.map
                         (fun line -> Str.split (Str.regexp "[ \t\r\n]+") line)
                         lines)
                  in
                  let lower_words = List.map String.lowercase_ascii words in
                  acc @ lower_words)
                [] record.all_files
            in

            (* Remove duplicates by sorting and getting unique words *)
            let unique_words = List.sort_uniq compare all_words in

            (* Sort words by length (longest first) *)
            let sorted_words =
              List.sort
                (fun a b -> compare (String.length b) (String.length a))
                unique_words
            in

            (* Take the longest n words *)
            let rec take n lst =
              match lst with
              | [] -> []
              | x :: xs when n > 0 -> x :: take (n - 1) xs
              | _ -> []
            in
            let longest_words = take n sorted_words in

            (* Build the summary string *)
            let header =
              Printf.sprintf
                "The longest %d unique words (case insensitive) across all \
                 files in record '%s':\n"
                n record_id
            in
            let words_str = String.concat "\n" longest_words in
            Printf.sprintf "%s%s\n" header words_str
          with Sys_error _ ->
            Printf.sprintf "Error: Could not read files in record '%s'.\n"
              record_id))

let longest_n_words_printer summary_str = print_string summary_str

let longest_n_words record_id n_str =
  generate_n_longest_words record_id n_str |> longest_n_words_printer

let generate_file_line_count record_id filename =
  let file_path = Printf.sprintf "data/%s/%s" record_id filename in
  try
    let lines = Server.read_file file_path in
    let line_count = List.length lines in
    Printf.sprintf "File '%s' in record '%s' has %d lines.\n" filename record_id
      line_count
  with Sys_error _ ->
    Printf.sprintf "Error: Could not read file '%s' in record '%s'.\n" filename
      record_id

let line_count_in_file_printer summary_str = print_string summary_str

let line_count_in_file record_id filename =
  generate_file_line_count record_id filename |> line_count_in_file_printer

let generate_avg_char_count id =
  try
    let record =
      Ocamlvcs__Server.read_records () |> List.find (fun r -> r.id = id)
    in
    let total_chars, total_files =
      List.fold_left
        (fun (char_sum, file_count) file_info ->
          let file_path = Printf.sprintf "data/%s/%s" id file_info.filename in
          try
            let content = Ocamlvcs__Server.read_file file_path in
            let char_count =
              List.fold_left
                (fun acc line -> acc + String.length line)
                0 content
            in
            (char_sum + char_count, file_count + 1)
          with Sys_error _ ->
            (* If we can't read a file, we just skip it *)
            (char_sum, file_count))
        (0, 0) record.all_files
    in
    if total_files = 0 then
      Printf.sprintf "No files found for record ID %s.\n" id
    else
      let avg = float_of_int total_chars /. float_of_int total_files in
      Printf.sprintf "Average character count for ID %s: %.2f\n" id avg
  with Not_found -> Printf.sprintf "Record with ID %s not found.\n" id

let avg_char_count_printer summary_str = print_string summary_str
let avg_char_count id = generate_avg_char_count id |> avg_char_count_printer

(* Count word occurrences in a file *)
let count_words file_path =
  try
    let content = Ocamlvcs__Server.read_file file_path in
    List.fold_left
      (fun acc line ->
        let words = Str.split (regexp "[ \t\n\r]+") line in
        List.fold_left
          (fun acc word ->
            let lower_word = String.lowercase_ascii word in
            if lower_word <> "" then
              if Hashtbl.mem acc lower_word then
                Hashtbl.replace acc lower_word (Hashtbl.find acc lower_word + 1)
              else Hashtbl.add acc lower_word 1;
            acc)
          acc words)
      (Hashtbl.create 100) content
  with Sys_error _ ->
    Printf.printf "Error reading file: %s\n" file_path;
    Hashtbl.create 0

let generate_top_n_words id n_str =
  match int_of_string_opt n_str with
  | None -> Printf.sprintf "Error: '%s' is not a valid number.\n" n_str
  | Some n when n <= 0 ->
      "Error: Please provide a positive number greater than 0.\n"
  | Some n -> (
      try
        let record =
          Ocamlvcs__Server.read_records () |> List.find (fun r -> r.id = id)
        in
        let count_words file_path =
          try
            let content = Ocamlvcs__Server.read_file file_path in
            let word_map = Hashtbl.create 100 in
            List.iter
              (fun line ->
                let words = Str.split (Str.regexp "[ \t\n\r]+") line in
                List.iter
                  (fun word ->
                    let lower_word = String.lowercase_ascii word in
                    if lower_word <> "" then
                      if Hashtbl.mem word_map lower_word then
                        Hashtbl.replace word_map lower_word
                          (Hashtbl.find word_map lower_word + 1)
                      else Hashtbl.add word_map lower_word 1)
                  words)
              content;
            word_map
          with Sys_error _ ->
            (* Return an empty map if file can't be read *)
            Hashtbl.create 0
        in

        let word_count = Hashtbl.create 100 in
        List.iter
          (fun file_info ->
            let file_path = Printf.sprintf "data/%s/%s" id file_info.filename in
            let file_word_count = count_words file_path in
            Hashtbl.iter
              (fun word count ->
                if Hashtbl.mem word_count word then
                  Hashtbl.replace word_count word
                    (Hashtbl.find word_count word + count)
                else Hashtbl.add word_count word count)
              file_word_count)
          record.all_files;

        let word_list =
          Hashtbl.fold
            (fun word count acc -> (word, count) :: acc)
            word_count []
        in
        let sorted_words =
          List.sort (fun (_, c1) (_, c2) -> compare c2 c1) word_list
        in

        let header = Printf.sprintf "Top %d words in ID %s:\n" n id in
        let top_words =
          sorted_words
          |> List.mapi (fun i (word, count) ->
                 if i < n then
                   Printf.sprintf "%d. %s: %d occurrences\n" (i + 1) word count
                 else "")
          |> List.filter (fun s -> s <> "")
        in
        header ^ String.concat "" top_words
      with Not_found -> Printf.sprintf "Record with ID %s not found.\n" id)

let top_n_words_printer summary_str = print_string summary_str
let top_n_words id n_str = generate_top_n_words id n_str |> top_n_words_printer
let count_n_words line = Str.split (Str.regexp "[ \t\n\r]+") line |> List.length

let generate_word_count record_id filename =
  let file_path = Printf.sprintf "data/%s/%s" record_id filename in
  try
    let lines = Server.read_file file_path in
    let count_n_words line =
      Str.split (Str.regexp "[ \t\n\r]+") line |> List.length
    in
    let total_words =
      List.fold_left (fun acc line -> acc + count_n_words line) 0 lines
    in
    Printf.sprintf "The file %s contains %d words.\n" file_path total_words
  with Sys_error err -> Printf.sprintf "Error: %s\n" err

let word_count_in_file_printer summary_str = print_string summary_str

let word_count_in_file record_id filename =
  generate_word_count record_id filename |> word_count_in_file_printer

let generate_insert_text_at_line record_id filename line_num_str text =
  match int_of_string_opt line_num_str with
  | None ->
      Error
        (Printf.sprintf "Error: Invalid line number format '%s'.\n" line_num_str)
  | Some line_num when line_num <= 0 ->
      Error "Error: Line number must be a positive integer.\n"
  | Some line_num -> (
      let file_path = Printf.sprintf "data/%s/%s" record_id filename in
      (* Attempt to read the file *)
      let lines_result =
        try Ok (Server.read_file file_path)
        with Sys_error err -> Error (Printf.sprintf "Error: %s\n" err)
      in
      match lines_result with
      | Error e -> Error e
      | Ok lines -> (
          if line_num > List.length lines + 1 then
            Error "Error: Invalid line number\n"
          else
            let before =
              List.rev
                (List.fold_left
                   (fun acc (i, line) ->
                     if i < line_num then line :: acc else acc)
                   []
                   (List.mapi (fun i x -> (i + 1, x)) lines))
            in
            let after =
              List.rev
                (List.fold_left
                   (fun acc (i, line) ->
                     if i >= line_num then line :: acc else acc)
                   []
                   (List.mapi (fun i x -> (i + 1, x)) lines))
            in
            let updated_lines = before @ (text :: after) in
            (* Attempt to write the updated file *)
            try
              let oc = open_out file_path in
              List.iteri
                (fun i line ->
                  if i = List.length updated_lines - 1 then
                    output_string oc line
                  else output_string oc (line ^ "\n"))
                updated_lines;
              close_out oc;
              Ok (line_num, file_path)
            with Sys_error err -> Error (Printf.sprintf "Error: %s\n" err)))

(* Called only in case of error *)
let insert_text_at_line_error_helper error_msg = error_msg

(* Prints based on success or error *)
let insert_text_at_line_printer result =
  match result with
  | Ok (line_num, file_path) ->
      Printf.printf "Inserted text at line %d in file: %s\n" line_num file_path
  | Error err ->
      let msg = insert_text_at_line_error_helper err in
      print_string msg

(* Main function *)
let insert_text_at_line record_id filename line_num_str text =
  generate_insert_text_at_line record_id filename line_num_str text
  |> insert_text_at_line_printer

(* Logic function: On success: returns Ok with a list of (file_path, replaced,
   target, replacement). On error: returns Error with an error message. *)
let generate_find_replace id target replacement filename_opt =
  try
    let record =
      Ocamlvcs__Server.read_records () |> List.find (fun r -> r.id = id)
    in
    let files_to_process =
      match filename_opt with
      | Some filename ->
          let filtered =
            List.filter (fun file -> file.filename = filename) record.all_files
          in
          if filtered = [] then
            (* Filename does not exist in the record *)
            raise
              (Failure
                 (Printf.sprintf "Error: File '%s' not found in record '%s'.\n"
                    filename id))
          else filtered
      | None -> record.all_files
    in

    let process_file file_info =
      let filename = file_info.filename in
      let file_path = Printf.sprintf "data/%s/%s" id filename in
      try
        let file_content = Ocamlvcs__Server.read_file file_path in
        let target_escaped = Str.quote target in
        let updated_content =
          List.map
            (fun line ->
              Str.global_replace
                (Str.regexp_string target_escaped)
                replacement line)
            file_content
        in
        if updated_content <> file_content then begin
          let output_channel = open_out file_path in
          List.iter
            (fun line -> output_string output_channel line)
            updated_content;
          close_out output_channel;
          (file_path, true, target, replacement)
        end
        else (file_path, false, target, replacement)
      with Sys_error err -> raise (Failure (Printf.sprintf "Error: %s\n" err))
    in

    let results =
      try List.map process_file files_to_process
      with Failure e -> raise (Failure e)
    in
    Ok results
  with
  | Not_found -> Error (Printf.sprintf "Record with ID %s not found.\n" id)
  | Failure e -> Error e

(* Called only in case of error *)
let find_replace_error_helper error_msg = error_msg

(* Prints based on success or error *)
let find_replace_printer result =
  match result with
  | Error err ->
      let msg = find_replace_error_helper err in
      print_string msg
  | Ok file_results ->
      List.iter
        (fun (file_path, replaced, target, replacement) ->
          if replaced then
            Printf.printf "Replaced '%s' with '%s' in file %s\n" target
              replacement file_path
          else Printf.printf "No matches found in file %s\n" file_path)
        file_results

(* Main function *)
let find_replace id target replacement filename_opt =
  generate_find_replace id target replacement filename_opt
  |> find_replace_printer
