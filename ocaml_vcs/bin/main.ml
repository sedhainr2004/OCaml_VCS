open Ocamlvcs.Server
open Ocamlvcs.Editor

let print_menu () =
  print_endline "\n---- Ocaml Git Commands ----";
  print_endline "-------------------------------------------";
  print_endline "1. give <filename1,filename2,...> <message>";
  print_endline "   Save a new version of files with a commit message.";
  print_endline "";
  print_endline "2. get <id>";
  print_endline "   Retrieve a specific version of files based on the given ID.";
  print_endline "";
  print_endline "3. del <id>";
  print_endline "   Delete a specific version based on the given ID.";
  print_endline "";
  print_endline "4. show";
  print_endline "   Display all saved versions of the files.";
  print_endline "";
  print_endline "5. diff <id1> <id2> <filename>";
  print_endline "   Show the differences between two versions of a file.";
  print_endline "";
  print_endline "6. changes";
  print_endline "   Check for changes in tracked files.";
  print_endline "";
  print_endline "---- Editor Commands ----";
  print_endline "-------------------------------------------";
  print_endline "7. find <id> <target> <replacement> <fileName> (optional)";
  print_endline
    "   Replace target text with replacement text in a file (optional).";
  print_endline "";
  print_endline "8. insert <id> <filename> <line_number> <text>";
  print_endline "   Insert text at the specified line number in the file.";
  print_endline "";
  print_endline "9. count <id> <filename>";
  print_endline "   Returns the word count for the specified file.";
  print_endline "";
  print_endline "10. avg <id>";
  print_endline
    "    Returns the average number of characters in all files of the record.";
  print_endline "";
  print_endline "11. common_words <id> <n>";
  print_endline
    "    Returns the 'n' most common words in all files of the record.";
  print_endline "";
  print_endline "12. longest_words <id> <n>";
  print_endline "    Returns the 'n' longest words in all files of the record.";
  print_endline "";
  print_endline "13. line_count <id> <filename>";
  print_endline "    Returns the number of lines in a specified file.";
  print_endline "";
  print_endline "14. summary <id> <filename>";
  print_endline
    "    Generates a summary for the file including word count, line count, \
     and character count.";
  print_endline "";
  print_endline "---- Other Commands ----";
  print_endline "-------------------------------------------";
  print_endline "15. explain <command>";
  print_endline "    Displays a detailed explanation of that command.";
  print_endline "";
  print_endline "16. quit";
  print_endline "    Exit GitCaml.";
  flush stdout

let explain_command cmd =
  match cmd with
  | "give" ->
      Printf.printf
        "Command: give <filename1,filename2,...> <message>\n\n\
         Description: Save a new version of files with a commit message.\n\
         This command is similar to 'git commit' in Git.\n\n\
         Usage: give file1.txt,file2.txt \"Initial commit\"\n\
         This saves a new version of file1.txt and file2.txt with the message \
         \"Initial commit\".\n\
         Each save is assigned a unique ID based on the current timestamp.\n"
  | "get" ->
      Printf.printf
        "Command: get <id>\n\n\
         Description: Retrieve a specific version of files based on the given \
         ID.\n\
         This command is similar to 'git checkout' in Git.\n\n\
         Usage: get 1234567890\n\
         This retrieves the version of files associated with ID 1234567890.\n"
  | "del" ->
      Printf.printf
        "Command: del <id>\n\n\
         Description: Delete a specific version based on the given ID.\n\
         This command is similar to 'git revert' in Git, but actually removes \
         the commit.\n\n\
         Usage: del 1234567890\n\
         This deletes the version with ID 1234567890 from the repository.\n"
  | "show" ->
      Printf.printf
        "Command: show\n\n\
         Description: Display all saved versions of the files.\n\
         This command is similar to 'git log' in Git.\n\n\
         Usage: show\n\
         This displays all saved versions with their IDs, dates, and messages.\n"
  | "diff" ->
      Printf.printf
        "Command: diff <id1> <id2> <filename>\n\n\
         Description: Show the differences between two versions of a file.\n\
         This command is similar to 'git diff' in Git.\n\n\
         Usage: diff 1234567890 9876543210 file.txt\n\
         This shows the differences in file.txt between versions 1234567890 \
         and 9876543210.\n\
         The diff algorithm uses Longest Common Subsequence (LCS) to identify \
         changes.\n"
  | "changes" ->
      Printf.printf
        "Command: changes\n\n\
         Description: Check for changes in tracked files.\n\
         This command is similar to 'git status' in Git.\n\n\
         Usage: changes\n\
         This displays the status of tracked files, showing which files have \
         been modified.\n"
  | "find" ->
      Printf.printf
        "Command: find <id> <target> <replacement> <filename> (optional)\n\n\
         Description: Replace target text with replacement text in a file (or \
         all files if filename is omitted).\n\
         This command is unique to GitCaml and doesn't have a direct Git \
         equivalent.\n\n\
         Usage: find 1234567890 \"old text\" \"new text\" file.txt\n\
         This replaces all occurrences of \"old text\" with \"new text\" in \
         file.txt of version 1234567890.\n"
  | "insert" ->
      Printf.printf
        "Command: insert <id> <filename> <line_number> <text>\n\n\
         Description: Insert text at the specified line number in the file.\n\
         This command is unique to GitCaml and doesn't have a direct Git \
         equivalent.\n\n\
         Usage: insert 1234567890 file.txt 5 \"New line of text\"\n\
         This inserts \"New line of text\" at line 5 in file.txt of version \
         1234567890.\n"
  | "count" ->
      Printf.printf
        "Command: count <id> <filename>\n\n\
         Description: Returns the word count for the specified file.\n\
         This command is unique to GitCaml and doesn't have a direct Git \
         equivalent.\n\n\
         Usage: count 1234567890 file.txt\n\
         This displays the total number of words in file.txt of version \
         1234567890.\n"
  | "avg" ->
      Printf.printf
        "Command: avg <id>\n\n\
         Description: Returns the average number of characters in all files of \
         the record.\n\
         This command is unique to GitCaml and doesn't have a direct Git \
         equivalent.\n\n\
         Usage: avg 1234567890\n\
         This calculates and displays the average character count across all \
         files in version 1234567890.\n"
  | "common_words" ->
      Printf.printf
        "Command: common_words <id> <n>\n\n\
         Description: Returns the 'n' most common words in all files of the \
         record.\n\
         This command is unique to GitCaml and doesn't have a direct Git \
         equivalent.\n\n\
         Usage: common_words 1234567890 10\n\
         This displays the 10 most frequently occurring words across all files \
         in version 1234567890.\n"
  | "longest_words" ->
      Printf.printf
        "Command: longest_words <id> <n>\n\n\
         Description: Returns the 'n' longest words in all files of the record.\n\
         This command is unique to GitCaml and doesn't have a direct Git \
         equivalent.\n\n\
         Usage: longest_words 1234567890 5\n\
         This displays the 5 longest words found across all files in version \
         1234567890.\n"
  | "line_count" ->
      Printf.printf
        "Command: line_count <id> <filename>\n\n\
         Description: Returns the number of lines in a specified file.\n\
         This command is unique to GitCaml and doesn't have a direct Git \
         equivalent.\n\n\
         Usage: line_count 1234567890 file.txt\n\
         This displays the total number of lines in file.txt of version \
         1234567890.\n"
  | "summary" ->
      Printf.printf
        "Command: summary <id> <filename>\n\n\
         Description: Generates a summary for the file including word count, \
         line count, and character count.\n\
         This command is unique to GitCaml and doesn't have a direct Git \
         equivalent.\n\n\
         Usage: summary 1234567890 file.txt\n\
         This displays a comprehensive summary of file.txt in version \
         1234567890, including various statistics.\n"
  | _ ->
      Printf.printf
        "Unknown command: %s\nUse 'help' to see a list of available commands.\n"
        cmd

let print_new_input () =
  print_newline ();
  print_endline "-------------------------------------------";
  print_string ">> "

let rec user_input () =
  match read_line () |> String.trim |> String.split_on_char ' ' with
  | "give" :: args -> (
      match args with
      | filenames :: message ->
          give (String.split_on_char ',' filenames) (String.concat " " message)
      | _ -> print_endline "Invalid 'give' command format")
  | "insert" :: args -> (
      (* Handle insert command with multi-word text in quotes *)
      try
        match args with
        | id :: filename :: line_num :: text_parts ->
            (* Join text parts, removing surrounding quotes if present *)
            let text =
              String.concat " " text_parts |> fun s ->
              if
                String.length s >= 2
                && s.[0] = '"'
                && s.[String.length s - 1] = '"'
              then String.sub s 1 (String.length s - 2)
              else s
            in
            insert_text_at_line id filename line_num text
        | _ -> print_endline "Invalid 'insert' command format"
      with _ -> print_endline "Error processing insert command")
  | [ "get"; id ] -> get id
  | [ "del"; id ] -> del id
  | [ "show" ] -> show ()
  | [ "diff"; id1; id2; filename ] -> diff id1 id2 filename
  | [ "changes" ] -> Ocamlvcs.Server.changes ()
  | [ "help" ] -> print_menu ()
  | [ "explain"; command ] -> explain_command command
  | [ "quit" ] -> raise Exit
  | [ "find"; id; target; replacement ] ->
      find_replace id target replacement None
  | [ "find"; id; target; replacement; filename ] ->
      find_replace id target replacement (Some filename)
  | [ "count"; id; filename ] -> word_count_in_file id filename
  | [ "avg"; id ] -> avg_char_count id
  | [ "common_words"; id; n ] -> top_n_words id n
  | [ "longest_words"; id; n ] -> longest_n_words id n
  | [ "line_count"; id; filename ] -> line_count_in_file id filename
  | [ "summary"; id; filename ] -> file_summary id filename
  | _ -> print_endline "Invalid command. Please try again."

let () =
  print_endline "Launching GitCaml, a lightweight version control system.";
  print_menu ();
  let rec loop () =
    print_new_input ();
    (try user_input ()
     with Exit ->
       print_endline "\nExiting GitCaml.";
       exit 0);
    loop ()
  in
  loop ()
