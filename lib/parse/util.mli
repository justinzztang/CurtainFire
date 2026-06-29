module StringMap : Map.S with type key = string

(**Stores a StringMap holding any type, and a pointer to the "next" node in the
   list. Essentially a linked list with each node having its own list*)
type 'a segmented_list = {
  mutable values : 'a StringMap.t;
  next : 'a segmented_list option;
}

(** Based on a given key, find [Some] value if it exists, [None] otherwise*)
val sl_find_opt : 'a segmented_list -> string -> 'a option

(** Create a string representation of a segmented_list based on a given printer
    function*)
val string_of_sl : 'a segmented_list -> (string * 'a -> string) -> string

type 'a tree_list =
  | Empty
  | Leaf of 'a
  | Join of 'a tree_list list

(**Flatten a tree_list into a list*)
val tree_to_list : 'a tree_list -> 'a list

(**Read user input as one string until "<end>" is inputted *)
val read_console : unit -> string
