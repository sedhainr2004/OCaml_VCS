(** Find and Replace Module *)

val insert_text_at_line : string -> string -> string -> string -> unit
(** [insert_text_at_line record_id filename line_num text] inserts [text] at
    line [line_num] in [filename] of record [record_id] *)

val find_replace : string -> string -> string -> string option -> unit
(** [find_replace id target replacement filename_opt] replaces [target] with
    [replacement] in files of record [id] *)

(** File Word Count *)

val word_count_in_file : string -> string -> unit
(** [word_count_in_file record_id filename] prints word count of [filename] in
    record [record_id] *)

(** Text Statistics Analyzer *)

val avg_char_count : string -> unit
(** [avg_char_count id] prints average character count of files in record [id] *)

val top_n_words : string -> string -> unit
(** [top_n_words id n] prints top [n] most common words in files of record [id] *)

(** File Statistics Module *)

val line_count_in_file : string -> string -> unit
(** [line_count_in_file record_id filename] prints line count of [filename] in
    record [record_id] *)

val count_words : string -> (string, int) Hashtbl.t
(** [count_words file_path] returns a hashtable of word counts in [file_path] *)

val longest_n_words : string -> string -> unit
(** [longest_n_words record_id n] prints [n] longest words in files of record
    [record_id] *)

val file_summary : string -> string -> unit
(** [file_summary record_id filename] prints summary of [filename] in record
    [record_id] *)

val generate_summary : string -> string -> string
(** [generate_summary record_id filename] returns summary string for [filename]
    in record [record_id] *)

val generate_n_longest_words : string -> string -> string
(** [generate_n_longest_words record_id n_str] returns string of [n_str] longest
    words in record [record_id] *)

val generate_file_line_count : string -> string -> string
(** [generate_file_line_count record_id filename] returns line count string for
    [filename] in record [record_id] *)

val generate_word_count : string -> string -> string
(** [generate_word_count record_id filename] returns word count string for
    [filename] in record [record_id] *)

val generate_top_n_words : string -> string -> string
(** [generate_top_n_words record_id n_str] returns string of top [n_str] words
    in record [record_id] *)

val generate_avg_char_count : string -> string
(** [generate_avg_char_count id] returns average character count string for
    files in record [id] *)

val generate_find_replace :
  string ->
  string ->
  string ->
  string option ->
  ((string * bool * string * string) list, string) result
(** [generate_find_replace id target replacement filename_opt] returns result of
    replacing [target] with [replacement] *)

val generate_insert_text_at_line :
  string -> string -> string -> string -> (int * string, string) result
(** [generate_insert_text_at_line record_id filename line_num text] returns
    result of inserting [text] at [line_num] *)
