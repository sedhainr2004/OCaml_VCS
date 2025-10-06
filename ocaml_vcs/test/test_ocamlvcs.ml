open OUnit2
open Ocamlvcs.Server
open Ocamlvcs.Editor
open FileUtil
open Ocamlvcs.Server

(* Generates a unique suffix for directories and files *)
let unique_suffix test_name = Printf.sprintf "%d_%s" (Unix.getpid ()) test_name

(* Sets up a unique testing environment *)
let setup_test_env suffix =
  let data_dir = "data_" ^ suffix in
  let records_file = "records_" ^ suffix ^ ".txt" in
  if Sys.file_exists data_dir then FileUtil.rm ~recurse:true [ data_dir ];
  if Sys.file_exists records_file then Sys.remove records_file;
  FileUtil.mkdir data_dir;
  (data_dir, records_file)

(* Cleans up the testing environment *)
let teardown_test_env (data_dir, records_file) =
  if Sys.file_exists data_dir then FileUtil.rm ~recurse:true [ data_dir ];
  if Sys.file_exists records_file then Sys.remove records_file

(* Creates a file with the given content *)
let create_test_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

(* Reads the content of a file *)
let read_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

(* Checks if a string contains a substring *)
let string_contains substr str =
  try
    let _ = Str.search_forward (Str.regexp_string substr) str 0 in
    true
  with Not_found -> false

(* Basic give test *)
let test_give _ =
  let suffix = unique_suffix "give" in
  let data_dir, records_file = setup_test_env suffix in
  let test_file = Filename.concat data_dir "test1.txt" in
  create_test_file test_file "Hello, GitCaml!";
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;
  give [ "test1.txt" ] "Initial commit";
  let records = read_records () in
  let verification_result =
    try
      assert_equal 1 (List.length records);
      let record = List.hd records in
      assert_equal "Initial commit" record.message;
      assert_equal 1 (List.length record.all_files);
      assert_equal 1 (List.length record.changed_files);
      assert_equal "test1.txt" (List.hd record.all_files).filename;
      true
    with _ -> false
  in
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Give operation failed verification" verification_result

(* Test give with empty file *)
let test_give_empty_file _ =
  let suffix = unique_suffix "give_empty" in
  let data_dir, records_file = setup_test_env suffix in
  let test_file = Filename.concat data_dir "empty.txt" in
  create_test_file test_file "";
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;
  give [ "empty.txt" ] "Empty file commit";
  let verification_result =
    try
      let records = read_records () in
      assert_equal 1 (List.length records);
      let record = List.hd records in
      assert_equal "Empty file commit" record.message;
      assert_equal "" (read_file "empty.txt");
      true
    with _ -> false
  in
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Give operation failed for empty file" verification_result

(* Test give with multiple files *)
let test_give_multiple_files _ =
  let suffix = unique_suffix "give_multiple" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;
  create_test_file "file1.txt" "Content 1";
  create_test_file "file2.txt" "Content 2";
  give [ "file1.txt"; "file2.txt" ] "Multiple files commit";
  let verification_result =
    try
      let records = read_records () in
      let record = List.hd records in
      assert_equal 2 (List.length record.changed_files);
      true
    with _ -> false
  in
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Give operation failed for multiple files" verification_result

(* Basic get test *)
let test_get _ =
  let suffix = unique_suffix "get" in
  let data_dir, records_file = setup_test_env suffix in
  create_test_file (Filename.concat data_dir "test2.txt") "Version 1";
  Sys.chdir data_dir;
  give [ "test2.txt" ] "First version";
  let records = read_records () in
  let id = (List.hd records).id in
  Sys.remove "test2.txt";
  get id;
  Sys.chdir "..";
  assert_equal "Version 1" (read_file (Filename.concat data_dir "test2.txt"));
  teardown_test_env (data_dir, records_file)

(* Test get with nonexistent ID *)
let test_get_nonexistent_id _ =
  let suffix = unique_suffix "get_nonexistent" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Capture output *)
  let output = ref "" in
  let old_stdout = Unix.dup Unix.stdout in
  let tmp_read, tmp_write = Unix.pipe () in
  Unix.dup2 tmp_write Unix.stdout;

  get "nonexistent_id";

  Unix.close tmp_write;
  output := input_line (Unix.in_channel_of_descr tmp_read);
  Unix.close tmp_read;
  Unix.dup2 old_stdout Unix.stdout;

  let verification_result = !output = "ID not found in records." in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Get operation should return 'ID not found in records.'"
    verification_result

(* Basic del test *)
let test_del _ =
  let suffix = unique_suffix "del" in
  let data_dir, records_file = setup_test_env suffix in
  create_test_file (Filename.concat data_dir "test4.txt") "Delete me";
  Sys.chdir data_dir;
  give [ "test4.txt" ] "To be deleted";
  let records = read_records () in
  let id = (List.hd records).id in
  del id;
  Sys.chdir "..";
  let updated_records = read_records () in
  assert_equal 0 (List.length updated_records);
  teardown_test_env (data_dir, records_file)

(* Test del with nonexistent ID *)
let test_del_nonexistent_id _ =
  let suffix = unique_suffix "del_nonexistent" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Capture output *)
  let output = ref "" in
  let old_stdout = Unix.dup Unix.stdout in
  let tmp_read, tmp_write = Unix.pipe () in
  Unix.dup2 tmp_write Unix.stdout;

  del "nonexistent_id";

  Unix.close tmp_write;
  output := input_line (Unix.in_channel_of_descr tmp_read);
  Unix.close tmp_read;
  Unix.dup2 old_stdout Unix.stdout;

  let verification_result = !output = "ID not found in records." in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Del operation should return 'ID not found in records.'"
    verification_result

let test_show_multiple_commits _ =
  let suffix = unique_suffix "show_refactored" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;
  (* Create test files and perform commits *)
  create_test_file "file1.txt" "Hello World";
  create_test_file "file2.txt" "Another file content";
  give [ "file1.txt" ] "First commit";
  give [ "file2.txt" ] "Second commit";
  (* Generate expected output using the refactored function *)
  let expected_output = generate_show () in
  (* Verify if expected output contains key details *)
  let verification_result =
    string_contains "First commit" expected_output
    && string_contains "Second commit" expected_output
    && string_contains "file1.txt" expected_output
    && string_contains "file2.txt" expected_output
  in
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Show output verification failed" verification_result

let test_show_single_commit _ =
  let suffix = unique_suffix "show_singlefile" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;
  (* Create a single test file and perform a commit *)
  create_test_file "singlefile.txt" "This is a single file commit.";
  give [ "singlefile.txt" ] "Single file commit";
  (* Generate the output from the show command *)
  let show_output = generate_show () in
  (* Verify if the output contains the expected commit details *)
  let verification_result =
    string_contains "Single file commit" show_output
    && string_contains "singlefile.txt" show_output
  in
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Show output for single file commit verification failed"
    verification_result

let test_show_no_commits _ =
  let suffix = unique_suffix "show_no_commits" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* No commits created *)
  let show_output = generate_show () in

  (* Verify the output header is present *)
  let verification_result =
    string_contains "ID" show_output
    && string_contains "Date" show_output
    && string_contains "Changed Files" show_output
    && string_contains "Message" show_output
    && not (string_contains "Commit" show_output)
  in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Show output with no commits verification failed"
    verification_result

let test_diff _ =
  let suffix = unique_suffix "diff" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create a file and commit as version 1 *)
  let filename = "testfile.txt" in
  create_test_file filename "Hello world\nThis is a test\nGoodbye!";
  give [ filename ] "Version 1 commit";

  (* Modify the same file and commit as version 2 *)
  create_test_file filename "Hello world\nThis is another test\nGoodbye!";
  give [ filename ] "Version 2 commit";

  (* Retrieve the committed records *)
  let records = read_records () in
  let record_v1 = List.nth records 0 in
  let record_v2 = List.nth records 1 in
  let id1 = record_v1.id in
  let id2 = record_v2.id in

  (* Generate the diff *)
  let diff_output = generate_diff id1 id2 filename in

  (* Normalize diff_output to a list of lines *)
  let diff_lines = String.split_on_char '\n' diff_output in

  (* Return to original directory and tear down environment *)
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  (* Verify the diff contains expected differences *)
  assert_bool "Diff should indicate a common line"
    (List.exists (fun line -> line = "  Hello world") diff_lines);
  assert_bool "Diff should indicate a line removed from v1"
    (List.exists (fun line -> line = "- This is a test") diff_lines);
  assert_bool "Diff should indicate a line added in v2"
    (List.exists (fun line -> line = "+ This is another test") diff_lines);
  assert_bool "Diff should indicate a common ending line"
    (List.exists (fun line -> line = "  Goodbye!") diff_lines)

let test_lcs _ =
  let suffix = unique_suffix "lcs" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  let verification_result =
    try
      let a = [| "a"; "b"; "c" |] in
      let b = [| "a"; "d"; "c" |] in
      let matrix = lcs a b in
      assert_equal 2 matrix.(3).(3);
      (* Length of LCS should be 2 *)
      true
    with _ -> false
  in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "LCS calculation failed" verification_result

let test_backtrack _ =
  let suffix = unique_suffix "backtrack" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  let verification_result =
    try
      let a = [| "a"; "b"; "c" |] in
      let b = [| "a"; "d"; "c" |] in
      let matrix = lcs a b in
      let diff_result = backtrack matrix a b in

      (* Check that common elements are marked correctly *)
      assert_equal (`Both, "a") (List.nth diff_result 0);
      (* Check that removed element is marked *)
      assert_equal (`Remove, "b") (List.nth diff_result 1);
      (* Check that added element is marked *)
      assert_equal (`Add, "d") (List.nth diff_result 2);
      (* Check final common element *)
      assert_equal (`Both, "c") (List.nth diff_result 3);
      true
    with _ -> false
  in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Backtrack operation failed" verification_result

(* Test changes function *)
let test_changes_no_versions _ =
  let suffix = unique_suffix "no_versions" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* No records are created *)
  let changes_output = generate_changes () in

  (* Return to original directory and tear down environment *)
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  (* Verify the output *)
  assert_equal ~msg:"No versions found"
    "No versions found. Unable to check for changes." changes_output

let test_changes_no_changes _ =
  let suffix = unique_suffix "no_changes" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create a file and commit it *)
  let filename = "testfile.txt" in
  create_test_file filename "Unchanged content";
  give [ filename ] "Initial commit";

  (* Create another version with the same content *)
  create_test_file filename "Unchanged content";
  give [ filename ] "Second commit";

  (* Generate changes *)
  let changes_output = generate_changes () in

  (* Return to original directory and tear down environment *)
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  (* Verify the output *)
  assert_equal ~msg:"No changes detected"
    "No changes detected in tracked files." changes_output

let test_changes_with_changes _ =
  let suffix = unique_suffix "with_changes" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create a file and commit it *)
  let filename = "testfile.txt" in
  create_test_file filename "Original content";
  give [ filename ] "First commit";

  (* Modify the file and commit again *)
  create_test_file filename "Modified content";
  give [ filename ] "Second commit";

  (* Generate changes *)
  let changes_output = generate_changes () in

  (* Return to original directory and tear down environment *)
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  (* Verify the output contains the filename *)
  assert_bool "Changes output should mention the changed file"
    (string_contains filename changes_output)
(* -------------------------------------------------------------------------------------------------------------------------------------------- *)

(* EDITOR TESTS *)
let test_top_n_words _ =
  let suffix = unique_suffix "top_n_words" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "test1.txt" "hello world hello\n";
  create_test_file "test2.txt" "hello test test world\n";
  give [ "test1.txt"; "test2.txt" ] "Multiple files commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let longest_words = generate_top_n_words record_id "2" in
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "hello should be included in longest words"
    (string_contains "hello" longest_words);
  assert_bool "world should be included in longest words"
    (string_contains "world" longest_words)

let test_top_n_words_invalid_record_id _ =
  let invalid_record_id = "nonexistent_record" in
  let result = generate_top_n_words invalid_record_id "3" in
  assert_bool "Expected error message to contain 'Record with ID'"
    (string_contains "Record with ID" result)

let test_top_n_words_invalid_filename _ =
  let suffix = unique_suffix "top_n_words_invalid_filename" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "valid_file.txt" "Hello world!";
  give [ "valid_file.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let result = generate_top_n_words record_id "invalid_file.txt" in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  assert_bool "Expected error message to contain 'Error'"
    (string_contains "Error" result)

let test_file_summary _ =
  let suffix = unique_suffix "file_summary" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create a test file and commit it *)
  create_test_file "testfile.txt" "Hello world\nThis is a test";
  give [ "testfile.txt" ] "Initial commit";

  (* Retrieve the committed record *)
  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  (* Generate the summary *)
  let summary = generate_summary record_id "testfile.txt" in

  (* Return to original directory and tear down environment *)
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  (* Verify that the summary contains expected fields *)
  assert_bool "Summary should contain line count"
    (string_contains "Line count:" summary);
  assert_bool "Summary should contain word count"
    (string_contains "Word count:" summary);
  assert_bool "Summary should contain character count"
    (string_contains "Character count:" summary)

let test_file_summary_invalid_record_id _ =
  let invalid_record_id = "nonexistent_record" in
  let result = generate_summary invalid_record_id "some_file.txt" in
  assert_bool "Expected error message to contain 'Record with ID'"
    (string_contains "Record with ID" result)

let test_file_summary_invalid_filename _ =
  let suffix = unique_suffix "file_summary_invalid_filename" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "valid_file.txt" "Some content";
  give [ "valid_file.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let result = generate_summary record_id "invalid_file.txt" in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  assert_bool "Expected error message to contain 'Error'"
    (string_contains "Error" result)

let test_line_count_in_file _ =
  let suffix = unique_suffix "line_count_in_file" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "testfile.txt" "Hello word\nThis is a test\nFor line count";
  give [ "testfile.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let line_count = generate_file_line_count record_id "testfile.txt" in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  assert_bool "Line count should be 3" (string_contains "3" line_count)

let test_word_count _ =
  let suffix = unique_suffix "word_count_in_file" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "testfile.txt" "Hello world\nThis is a test";

  give [ "testfile.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let word_count = generate_word_count record_id "testfile.txt" in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  assert_bool "Word count must be 6" (string_contains "6" word_count)

let test_word_count_in_file_invalid_record_id _ =
  let invalid_record_id = "nonexistent_record" in
  let result = generate_word_count invalid_record_id "some_file.txt" in
  assert_bool "Expected error message to contain 'Record with ID'"
    (string_contains "Record with ID" result)

let test_word_count_in_file_invalid_filename _ =
  let suffix = unique_suffix "word_count_invalid_filename" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "valid_file.txt" "Some content";
  give [ "valid_file.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let result = generate_word_count record_id "invalid_file.txt" in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  assert_bool "Expected error message to contain 'Error'"
    (string_contains "Error" result)

let test_avg_char_count _ =
  let suffix = unique_suffix "avg_char_count" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "file1.txt" "Hello world!";
  create_test_file "file2.txt" "The quick brown fox jumped";

  give [ "file1.txt"; "file2.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let avg_char_count = generate_avg_char_count record_id in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  assert_bool "Avg char count must be 19" (string_contains "19" avg_char_count)

let test_avg_char_count_invalid_record_id _ =
  let invalid_record_id = "nonexistent_record" in
  let result = generate_avg_char_count invalid_record_id in
  assert_bool "Expected error message to contain 'Record with ID'"
    (string_contains "Record with ID" result)

let test_count_words _ =
  let suffix = unique_suffix "count_words" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create a test file with known word frequencies *)
  (* Words: hello(3), world(2), test(1) *)
  let file_name = "words.txt" in
  create_test_file file_name "hello world\nhello test\nhello world\n";

  (* Call the count_words helper directly *)
  let file_path = Filename.concat (Sys.getcwd ()) file_name in
  let word_map = count_words file_path in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  (* Verify the counts *)
  (* 'hello' should appear 3 times *)
  let hello_count =
    if Hashtbl.mem word_map "hello" then Hashtbl.find word_map "hello" else 0
  in
  (* 'world' should appear 2 times *)
  let world_count =
    if Hashtbl.mem word_map "world" then Hashtbl.find word_map "world" else 0
  in
  (* 'test' should appear 1 time *)
  let test_count =
    if Hashtbl.mem word_map "test" then Hashtbl.find word_map "test" else 0
  in

  assert_equal 3 hello_count ~msg:"'hello' count should be 3";
  assert_equal 2 world_count ~msg:"'world' count should be 2";
  assert_equal 1 test_count ~msg:"'test' count should be 1"

let test_longest_n_words _ =
  let suffix = unique_suffix "avg_char_count" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "file1.txt" "Hello world";
  create_test_file "file2.txt" "The quick brown fox jumped";

  give [ "file1.txt"; "file2.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let longest_n_words = generate_n_longest_words record_id "2" in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  assert_bool "Longest 2 words must contain jumped"
    (string_contains "jumped" longest_n_words);
  assert_bool "Longest 2 words must contain brown"
    (string_contains "brown" longest_n_words)

let test_longest_n_words_invalid_record_id _ =
  let invalid_record_id = "nonexistent_record" in
  let result = generate_n_longest_words invalid_record_id "5" in
  assert_bool "Expected error message to contain 'Record with ID'"
    (string_contains "Record with ID" result)

let test_longest_n_words_invalid_filename _ =
  let suffix = unique_suffix "longest_n_words_invalid_filename" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "valid_file.txt" "Some content";
  give [ "valid_file.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let result = generate_n_longest_words record_id "invalid_file.txt" in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  assert_bool "Expected error message to contain 'Error'"
    (string_contains "Error" result)

(* Test find_replace function *)
let test_find_replace_for_all_files _ =
  let suffix = unique_suffix "find_replace" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create test files with the word "the" in them *)
  create_test_file "file1.txt"
    "The quick little fox jumped over the brown fence the fence";
  create_test_file "file2.txt"
    "The quick little fox jumped over the brown fence the fence\nthe fox landed";

  (* Commit the files to a new record *)
  give [ "file1.txt"; "file2.txt" ] "Initial commit";

  (* Get the record ID for later use *)
  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  (* Perform find_replace on the record, replacing "the" with "thee" *)
  find_replace record_id "the" "thee" None;

  (* Remove the current working copies so we can test retrieving updated
     versions *)
  if Sys.file_exists "file1.txt" then Sys.remove "file1.txt";
  if Sys.file_exists "file2.txt" then Sys.remove "file2.txt";

  (* Use get to restore the files from the record after replacement *)
  get record_id;

  (* Now read the content of the files from the working directory *)
  let file1_content = read_file "file1.txt" in
  let file2_content = read_file "file2.txt" in

  (* Check that "thee" is present and "the" is not *)
  assert_bool "file1.txt should contain 'thee'"
    (string_contains "thee" file1_content);
  assert_bool "file1.txt should not contain ' the '"
    (not (string_contains " the " file1_content));

  assert_bool "file2.txt should contain 'thee'"
    (string_contains "thee" file2_content);
  assert_bool "file2.txt should not contain ' the '"
    (not (string_contains " the " file2_content));

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_find_replace_nonexistent_file _ =
  let suffix = unique_suffix "find_replace_nonexistent_file_logic" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create a file and commit it to create a record *)
  create_test_file "existing_file.txt" "Just some text";
  give [ "existing_file.txt" ] "Initial commit";

  (* Retrieve the record ID *)
  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  (* We'll call find_replace_logic on a filename that doesn't exist in the
     record *)
  let nonexistent_filename = "nonexistent.txt" in

  (* Directly call find_replace_logic to get the result *)
  let result =
    generate_find_replace record_id "old" "new" (Some nonexistent_filename)
  in

  (* Return to original directory and tear down environment *)
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  (* Assert that the result is an Error and that it contains an error message *)
  match result with
  | Ok _ -> assert_failure "Expected an error result but got Ok."
  | Error err_msg ->
      assert_bool "Expected error message to contain 'Error:'"
        (string_contains "Error:" err_msg)

let test_find_replace_nonexistent_record _ =
  let invalid_record_id = "123456" in

  (* Directly call find_replace_logic to get the result *)
  let result = generate_find_replace invalid_record_id "thee" "the" None in

  (* Assert that the result is an Error and that it contains an error message *)
  match result with
  | Ok _ -> assert_failure "Expected an error result but got Ok."
  | Error err_msg ->
      assert_bool "Record with ID should be contained in the message"
        (string_contains "Record with ID" err_msg)

let test_find_replace_in_single_file _ =
  let suffix = unique_suffix "find_replace_single_file" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create two files with "the" in them *)
  create_test_file "file1.txt"
    "The quick little fox jumped over the brown fence the fence";
  create_test_file "file2.txt"
    "The quick little fox jumped over the brown fence the fence\nthe fox landed";

  (* Commit the files *)
  give [ "file1.txt"; "file2.txt" ] "Initial commit";

  (* Get the record ID *)
  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  (* Perform find_replace on just file1.txt, replacing "the" with "thee" *)
  find_replace record_id "the" "thee" (Some "file1.txt");

  (* Remove current working files to ensure we fetch the updated versions *)
  if Sys.file_exists "file1.txt" then Sys.remove "file1.txt";
  if Sys.file_exists "file2.txt" then Sys.remove "file2.txt";

  (* Retrieve files after replacement *)
  get record_id;

  (* Read the updated contents *)
  let file1_content = read_file "file1.txt" in
  let file2_content = read_file "file2.txt" in

  (* Check that file1.txt now contains "thee" and no longer " the " *)
  assert_bool "file1.txt should contain 'thee'"
    (string_contains "thee" file1_content);
  assert_bool "file1.txt should not contain ' the '"
    (not (string_contains " the " file1_content));

  (* Check that file2.txt should be unchanged *)
  assert_bool "file2.txt should still contain ' the '"
    (string_contains " the " file2_content);
  assert_bool "file2.txt should not contain 'thee'"
    (not (string_contains "thee" file2_content));

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_insert_text_at_valid_line _ =
  (* Test inserting text in the middle of the file (e.g., line 2) *)
  let suffix = unique_suffix "insert_line_valid" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "file.txt" "Alpha\nBeta\nGamma";
  give [ "file.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  (* Insert text at line 2, pushing existing lines down *)
  insert_text_at_line record_id "file.txt" "2" "Inserted in middle";

  Sys.remove "file.txt";
  get record_id;

  let updated_content = read_file "file.txt" in
  let updated_lines = String.split_on_char '\n' updated_content in
  assert_equal "Alpha" (List.nth updated_lines 0);
  assert_equal "Inserted in middle" (List.nth updated_lines 1);
  assert_equal "Beta" (List.nth updated_lines 2);
  assert_equal "Gamma" (List.nth updated_lines 3);

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

(* Test insert_text_at_line_logic with an invalid record ID *)
let test_insert_text_at_line_invalid_record_id _ =
  (* We do not create or commit any file, so no record exists. Use a made-up
     record_id and filename. *)
  let record_id = "nonexistent_record" in
  let filename = "file.txt" in
  let line_num_str = "1" in
  let text = "New line of text" in

  let result =
    generate_insert_text_at_line record_id filename line_num_str text
  in

  match result with
  | Ok _ -> assert_failure "Expected an error result but got Ok."
  | Error err_msg ->
      (* Check that the error message indicates a problem reading the file or
         invalid record *)
      assert_bool "Expected error message to contain 'Error:'"
        (string_contains "Error:" err_msg)

(* Test insert_text_at_line_logic with a valid record but invalid filename *)
let test_insert_text_at_line_invalid_filename _ =
  let suffix = unique_suffix "insert_line_invalid_filename" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create and commit a file to get a valid record ID *)
  create_test_file "valid_file.txt" "Some content";
  give [ "valid_file.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  (* Try inserting text into a filename that doesn't exist in the record *)
  let filename = "invalid_file.txt" in
  let line_num_str = "1" in
  let text = "Inserted text" in

  let result =
    generate_insert_text_at_line record_id filename line_num_str text
  in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  match result with
  | Ok _ -> assert_failure "Expected an error result but got Ok."
  | Error err_msg ->
      (* Check that the error message indicates a problem reading the file *)
      assert_bool "Expected error message to contain 'Error:'"
        (string_contains "Error:" err_msg)

(* Test 'longest_n_words' with invalid n *)
let test_longest_n_words_invalid_n _ =
  let suffix = unique_suffix "longest_n_words_invalid_n" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;
  create_test_file "test.txt" "Some content";
  give [ "test.txt" ] "Initial commit";
  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in
  let result = generate_n_longest_words record_id "-1" in
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Expected error message for invalid n"
    (string_contains "Error: The value of 'n' must be a positive integer" result)

(* Test 'top_n_words' with invalid n *)
let test_top_n_words_invalid_n _ =
  let suffix = unique_suffix "top_n_words_invalid_n" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;
  create_test_file "test.txt" "Some content";
  give [ "test.txt" ] "Initial commit";
  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in
  let result = generate_top_n_words record_id "0" in
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_bool "Expected error message for invalid n"
    (string_contains "Error: Please provide a positive number greater than 0"
       result)

(* Test 'count_words' with file reading error *)
let test_count_words_file_error _ =
  let suffix = unique_suffix "count_words_file_error" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;
  let non_existent_file = Filename.concat (Sys.getcwd ()) "non_existent.txt" in
  let word_map = count_words non_existent_file in
  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);
  assert_equal 0 (Hashtbl.length word_map)
    ~msg:"Word map should be empty for non-existent file"

let test_insert_text_at_invalid_line_number _ =
  let suffix = unique_suffix "insert_line_invalid_number" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "test_file.txt" "Line 1\nLine 2\nLine 3";
  give [ "test_file.txt" ] "Initial commit";

  let records = read_records () in
  let record = List.hd records in
  let record_id = record.id in

  let result =
    generate_insert_text_at_line record_id "test_file.txt" "5"
      "This should fail"
  in

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file);

  match result with
  | Ok _ -> assert_failure "Expected an error result but got Ok."
  | Error err_msg ->
      assert_equal "Error: Invalid line number\n" err_msg
        ~msg:"Expected error message for invalid line number"

(* ------------------------------------------------------------------------------------------------------------------------------------------------- *)

(* SERVER TESTS *)
let test_parse_invalid_file_info _ =
  let suffix = unique_suffix "parse_invalid_file_info" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create a record with invalid file info *)
  let invalid_record =
    "123,Test Message,2023-01-01,invalid_file_info,valid:hash\n"
  in
  let oc = open_out records_file in
  output_string oc invalid_record;
  close_out oc;

  let records = read_records () in

  assert_equal 0 (List.length records)
    ~msg:"Invalid record should not be parsed";

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_parse_invalid_record_format _ =
  let suffix = unique_suffix "parse_invalid_record_format" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create a record with invalid format *)
  let invalid_record = "123,Test Message,2023-01-01\n" in
  let oc = open_out records_file in
  output_string oc invalid_record;
  close_out oc;

  let records = read_records () in

  assert_equal 0 (List.length records)
    ~msg:"Invalid record format should not be parsed";

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_del_write_record _ =
  let suffix = unique_suffix "del_write_record" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Create two records *)
  let record1 =
    {
      id = "123";
      message = "Test Message 1";
      date = "2023-01-01";
      all_files = [ { filename = "file1.txt"; hash = "abc123" } ];
      changed_files = [ { filename = "file1.txt"; hash = "abc123" } ];
    }
  in
  let record2 =
    {
      id = "456";
      message = "Test Message 2";
      date = "2023-01-02";
      all_files = [ { filename = "file2.txt"; hash = "def456" } ];
      changed_files = [ { filename = "file2.txt"; hash = "def456" } ];
    }
  in

  (* Write records to the file *)
  let oc = open_out records_file in
  Printf.fprintf oc "%s,%s,%s,%s,%s\n" record1.id record1.message record1.date
    "file1.txt:abc123" "file1.txt:abc123";
  Printf.fprintf oc "%s,%s,%s,%s,%s\n" record2.id record2.message record2.date
    "file2.txt:def456" "file2.txt:def456";
  close_out oc;

  (* Delete the first record *)
  del "123";

  (* Ensure the file exists before reading *)
  assert_bool "Records file should exist" (Sys.file_exists records_file);

  (* Read the records file and check its contents *)
  let content = read_file records_file in

  (* Check that only the second record remains *)
  assert_bool "Only second record should remain"
    (string_contains
       "456,Test Message 2,2023-01-02,file2.txt:def456,file2.txt:def456" content);

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_give_non_existent_files _ =
  let suffix = unique_suffix "give_non_existent" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "existing.txt" "I exist";
  give [ "existing.txt"; "non_existent.txt" ] "Commit with non-existent file";

  let records = read_records () in
  assert_equal 1 (List.length records)
    ~msg:"Should create a record even with non-existent files";
  let record = List.hd records in
  assert_equal 1
    (List.length record.all_files)
    ~msg:"Should only include existing file";
  assert_equal "existing.txt" (List.hd record.all_files).filename
    ~msg:"Should include existing file";

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_give_and_get _ =
  let suffix = unique_suffix "give_and_get" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "test.txt" "Hello, GitCaml!";
  give [ "test.txt" ] "Initial commit";

  let records = read_records () in
  let id = (List.hd records).id in

  Sys.remove "test.txt";
  get id;

  let content = read_file "test.txt" in
  assert_equal "Hello, GitCaml!" content
    ~msg:"File content should match after get";

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_changes_after_modification _ =
  let suffix = unique_suffix "changes_after_mod" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  create_test_file "test.txt" "Original content";
  give [ "test.txt" ] "Initial commit";

  create_test_file "test.txt" "Modified content";
  let changes_output = generate_changes () in

  assert_bool "Changes output should mention the modified file"
    (string_contains "test.txt" changes_output);

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_diff_identical_files _ =
  let suffix = unique_suffix "diff_identical" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  let filename = "testfile.txt" in
  create_test_file filename "Identical content";
  give [ filename ] "Version 1";
  give [ filename ] "Version 2";

  let records = read_records () in
  let id1 = (List.nth records 1).id in
  let id2 = (List.nth records 0).id in

  let diff_output = generate_diff id1 id2 filename in

  assert_bool "Diff should indicate no differences"
    (string_contains "Diff between testfile.txt (version" diff_output
    && string_contains " Identical content" diff_output);

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_concurrent_version_creation _ =
  let suffix = unique_suffix "concurrent_creation" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  (* Simulate concurrent version creation *)
  let file1 = "file1.txt" in
  let file2 = "file2.txt" in
  create_test_file file1 "Content 1";
  create_test_file file2 "Content 2";

  let id1 = give [ file1 ] "First commit" in
  Unix.sleepf 0.001;
  (* Ensure a slight delay *)
  let id2 = give [ file2 ] "Second commit" in

  assert_bool "IDs should be different" (id1 <> id2);

  let records = read_records () in
  assert_equal 2 (List.length records) ~msg:"Should have two distinct records";

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_revert_to_previous_version _ =
  let suffix = unique_suffix "revert" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  let filename = "test.txt" in
  create_test_file filename "Version 1";
  give [ filename ] "First version";
  let id1 = (read_records () |> List.hd).id in

  create_test_file filename "Version 2";
  give [ filename ] "Second version";

  get id1;

  (* Revert to first version *)
  let content = read_file filename in
  assert_equal "Version 1" content
    ~msg:"File should be reverted to first version";

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_revert_to_previous_version _ =
  let suffix = unique_suffix "revert" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  let filename = "test.txt" in
  create_test_file filename "Version 1";
  give [ filename ] "First version";

  (* Get the ID of the first version *)
  let records = read_records () in
  let id1 = (List.hd records).id in

  create_test_file filename "Version 2";
  give [ filename ] "Second version";

  get id1;

  (* Revert to first version *)
  let content = read_file filename in
  assert_equal "Version 1" content
    ~msg:"File should be reverted to first version";

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_special_characters_in_filename _ =
  let suffix = unique_suffix "special_chars" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  let filename = "special_!@#$%^&*()_file.txt" in
  create_test_file filename "Special content";

  give [ filename ] "Special filename commit";

  (* Get the record ID *)
  let records = read_records () in
  let id = (List.hd records).id in

  Sys.remove filename;
  get id;

  assert_bool "File with special characters should exist"
    (Sys.file_exists filename);
  let content = read_file filename in
  assert_equal "Special content" content ~msg:"Content should be preserved";

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

let test_multiple_deletions_and_recreations _ =
  let suffix = unique_suffix "del_recreate" in
  let data_dir, records_file = setup_test_env suffix in
  let original_dir = Sys.getcwd () in
  Sys.chdir data_dir;

  let filename = "test.txt" in
  create_test_file filename "Version 1";
  give [ filename ] "First version";
  let id1 = (read_records () |> List.hd).id in

  del id1;

  create_test_file filename "Version 2";
  give [ filename ] "Recreated version";
  let id2 = (read_records () |> List.hd).id in

  del id2;

  create_test_file filename "Version 3";
  give [ filename ] "Final version";
  let id3 = (read_records () |> List.hd).id in

  let records = read_records () in
  assert_equal 1 (List.length records)
    ~msg:"Should only have one record after deletions";
  assert_equal id3 (List.hd records).id
    ~msg:"Final version should be the only remaining record";

  Sys.chdir original_dir;
  teardown_test_env (data_dir, records_file)

(* ------------------------------------------------------------------------------------------------------------------------------------------------- *)

(* Test suite *)
let suite =
  "GitCaml Test Suite"
  >::: [
         (*---------Server Tests---------*)
         (* give related *)
         "test_give_basic" >:: test_give;
         "test_give_empty_file" >:: test_give_empty_file;
         "test_give_multiple_files" >:: test_give_multiple_files;
         "test_give_non_existent_files" >:: test_give_non_existent_files;
         "test_give_and_get" >:: test_give_and_get;
         (* get related *)
         "test_get_basic" >:: test_get;
         "test_get_nonexistent_id" >:: test_get_nonexistent_id;
         (* del related *)
         "test_del_basic" >:: test_del;
         "test_del_nonexistent_id" >:: test_del_nonexistent_id;
         "test_del_write_record" >:: test_del_write_record;
         (* show related *)
         "test show_no_commits" >:: test_show_no_commits;
         "test_show_multiple_commits" >:: test_show_multiple_commits;
         "test_show_single_commit" >:: test_show_single_commit;
         (* changes related *)
         "test_changes_no_versions" >:: test_changes_no_versions;
         "test_changes_no_changes" >:: test_changes_no_changes;
         "test_changes_with_changes" >:: test_changes_with_changes;
         "test_changes_after_modification" >:: test_changes_after_modification;
         (* diff related *)
         "test_diff" >:: test_diff;
         "test_diff_identical_files" >:: test_diff_identical_files;
         (* lcs/backtrack related *)
         "test_lcs" >:: test_lcs;
         "test_backtrack" >:: test_backtrack;
         (* parsing/reading records related *)
         "test_parse_invalid_file_info" >:: test_parse_invalid_file_info;
         "test_parse_invalid_record_format" >:: test_parse_invalid_record_format;
         (*---------Editor Tests----------*)
         (* file_summary related *)
         "test_file_summary_valid_implementation" >:: test_file_summary;
         "test_file_summary_invalid_record_id"
         >:: test_file_summary_invalid_record_id;
         "test_file_summary_invalid_filename"
         >:: test_file_summary_invalid_filename;
         (* top_n_words related *)
         "test_top_n_words" >:: test_top_n_words;
         "test_top_n_words_invalid_record_id"
         >:: test_top_n_words_invalid_record_id;
         "test_top_n_words_invalid_filename"
         >:: test_top_n_words_invalid_filename;
         "test_top_n_words_invalid_n" >:: test_top_n_words_invalid_n;
         (* line_count_in_file related *)
         "test_line_count_in_file" >:: test_line_count_in_file;
         (* word_count_in_file related *)
         "test_word_count_in_file" >:: test_word_count;
         "test_count_words_file_error" >:: test_count_words_file_error;
         (* avg_char_count related *)
         "test_avg_char_count" >:: test_avg_char_count;
         "test_avg_char_count_invalid_record_id"
         >:: test_avg_char_count_invalid_record_id;
         (* longest_n_words related *)
         "test_longest_n_words" >:: test_longest_n_words;
         "test_longest_n_words_invalid_filename"
         >:: test_longest_n_words_invalid_filename;
         "test_longest_n_words_invalid_record_id"
         >:: test_longest_n_words_invalid_record_id;
         "test_longest_n_words_invalid_n" >:: test_longest_n_words_invalid_n;
         (* find_replace related *)
         "test_find_replace_for_all_files" >:: test_find_replace_for_all_files;
         "test_find_replace_in_single_file" >:: test_find_replace_in_single_file;
         "test_find_replace_nonexistent_file"
         >:: test_find_replace_nonexistent_file;
         "test_find_replace_nonexistent_record"
         >:: test_find_replace_nonexistent_record;
         (* insert_text_at_line related *)
         "test_insert_text_at_valid_line" >:: test_insert_text_at_valid_line;
         "test_insert_text_at_line_invalid_record_id"
         >:: test_insert_text_at_line_invalid_record_id;
         "test_insert_text_at_line_invalid_filename"
         >:: test_insert_text_at_line_invalid_filename;
         "test_insert_text_at_invalid_line_number"
         >:: test_insert_text_at_invalid_line_number;
         (* count_words *)
         "test_count_words" >:: test_count_words;
       ]

(* Run tests *)
let () = run_test_tt_main suite
