exception NotImplemented 

(*** HELPERS ***)

type board = int array array

(* FOR Q1 *)
let is_valid_list arr n =
  let arr_copy = Array.copy arr in  (* Create a copy of the array *)
  Array.sort compare arr_copy;  (* Sort the copy in-place *)
  arr_copy = Array.init (n * n) (fun i -> i + 1)  (* Compare sorted array with expected array *)

(* FOR Q2 *)
let print_board board =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      Printf.printf "%d " cell
    ) row;
    print_newline ()
  ) board




(*** QUESTION 1: VALDIATOR ***)

(* TODO: Implement get_row *)
let get_row board row =
  raise NotImplemented

(* TODO: Implement get_column *)
let get_column board col n =
  raise NotImplemented

(* TODO: Implement get_square *)
let get_square board row col n =
  raise NotImplemented

(* TODO: Implement is_valid_board *)
let full_valid_board board n =
  raise NotImplemented




(*** QUESTION 2: BRUTE FORCE SOLVER ***)

(* TODO: Implement row_valid *)
let row_valid board row value =
  raise NotImplemented

(* TODO: Implement col_valid *)
let col_valid board col value =
  raise NotImplemented

(* TODO: Implement square_valid *)
let square_valid board row col value n =
  raise NotImplemented

(* TODO: Implement board_valid *)
let check_board_valid board row col value n =
  raise NotImplemented

(* TODO: Implement find_empty *)
let find_empty board n =
  raise NotImplemented

(* TODO: Implement solve *)
let rec solve board n =
  raise NotImplemented


(*** QUESTION 3: FASTER ALGORITHM (ninja level) ***)

