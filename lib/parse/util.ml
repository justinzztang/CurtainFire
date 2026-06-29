module StringMap = Map.Make (String)

type 'a segmented_list = {
  mutable values : 'a StringMap.t;
  next : 'a segmented_list option;
}

let rec sl_find_opt (sl : 'a segmented_list) key =
  let elem = StringMap.find_opt key sl.values in
  match sl.next with
  | None -> elem
  | Some sln -> (
      match elem with
      | Some e -> Some e
      | None -> sl_find_opt sln key)

let rec string_of_sl sl val_printer =
  let rec list_string l printer =
    StringMap.fold (fun k v acc -> printer (k, v) ^ acc) l ""
  in
  match sl.next with
  | None -> list_string sl.values val_printer
  | Some sln -> list_string sl.values val_printer ^ string_of_sl sln val_printer

type 'a tree_list =
  | Empty
  | Leaf of 'a
  | Join of 'a tree_list list

let tree_to_list tree =
  let rec helper t acc =
    match t with
    | Empty -> acc
    | Leaf v -> v :: acc
    | Join vs -> List.fold_right (fun v acc -> helper v acc) vs acc
  in
  helper tree []

let read_console () : string =
  let rec read_lines (s : string) : string =
    let input = read_line () in
    try
      let i = Str.search_forward (Str.regexp "<end>") input 0 in
      s ^ String.sub input 0 i ^ "\n"
    with Not_found -> read_lines (s ^ input ^ "\n")
  in
  read_lines ""
