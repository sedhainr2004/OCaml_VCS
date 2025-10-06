(** [file_info] represents information about a tracked file *)
type file_info = {
  filename : string; (** The name of the file *)
  hash : string;     (** A unique hash representing the file's content *)
}

(** [record] represents a version (commit) in the version control system *)
type record = {
  id : string;           (** A unique identifier for the version *)
  message : string;      (** The commit message describing the changes *)
  date : string;         (** The timestamp of the version *)
  all_files : file_info list;     (** List of all files in the version *)
  changed_files : file_info list; (** List of files that were modified in this version *)
}

val read_records : unit -> record list
(** [read_records ()] reads the record list currently stored in records.txt *)

val write_record : record -> unit
(** [write_record record] appends the given [record] to records.txt *)

val generate_id : unit -> string
(** [generate_id ()] generates a unique identifier for a new version *)

val get_all_tracked_files : unit -> string list
(** [get_all_tracked_files ()] retrieves the list of all tracked files *)

val give : string list -> string -> unit
(** [give filenames message] stores a new version of [filenames] with [message] *)

val get : string -> unit
(** [get id] retrieves the version with [id] and updates the master file *)

val generate_show : unit -> string
(** [generate_show ()] generates a string representation of version history *)

val show : unit -> unit
(** [show ()] displays the history of versions *)

val del : string -> unit
(** [del id] deletes the version with [id] from records and data directory *)

val read_file : string -> string list
(** [read_file filename] reads contents of [filename] as a list of strings *)

val lcs : string array -> string array -> int array array
(** [lcs a b] computes the Longest Common Subsequence matrix for arrays [a] and
    [b] *)

val backtrack :
  int array array ->
  string array ->
  string array ->
  ([ `Add | `Both | `Remove ] * string) list
(** [backtrack matrix a b] generates a list of diff operations from LCS matrix *)

val generate_diff : string -> string -> string -> string
(** [generate_diff id1 id2 filename] generates diff between [filename] in
    versions [id1] and [id2] *)

val diff : string -> string -> string -> unit
(** [diff id1 id2 filename] displays differences between [filename] in versions
    [id1] and [id2] *)

val generate_changes : unit -> string
(** [generate_changes ()] generates a string describing changes in tracked files *)

val changes : unit -> unit
(** [changes ()] displays changes in tracked files since last commit *)
