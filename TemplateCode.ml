exception NotImplemented 

(*** HELPERS ***)
type board = int array array

(* FOR Q1 *)
let is_valid_list (arr : int array) (n : int) : bool =
  let arr_copy = Array.copy arr in 
  Array.sort compare arr_copy;  (* sort copy in-place *)
  arr_copy = Array.init (n * n) (fun i -> i + 1)  (* compare sorted array with what it should be *)

(* FOR Q2 *)
let print_board (board : int array array) : _ =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      Printf.printf "%d " cell
    ) row;
    print_newline ()
  ) board

(* FOR Q3 *)

(*
**Uncomment the following when running the code with Z3 installed:**

let create_solver ctx =
  let solver = Z3.Solver.mk_solver ctx None in
  solver

let print_solution grid model =
    Array.iter (fun row ->
      Array.iter (fun cell ->
          (* get value of each cell*)
          let eval_result = Z3.Model.eval model cell true in
          match eval_result with
          | Some expr ->
              let value = Z3.Expr.to_string expr in
              Printf.printf "%s " value
          | None ->
              Printf.printf "0 "  
      ) row;
      Printf.printf "\n"  
  ) grid;;
*)


(*** QUESTION 1: VALDIATOR ***)

(* TODO: Implement get_row *)
let get_row (board : int array array) (row : int array) : int array =
  raise NotImplemented

(* TODO: Implement get_column *)
let get_column (board : int array array) (col : int array) (n : int) : int array =
  raise NotImplemented

(* TODO: Implement get_square *)
let get_square (board : int array array) (row : int array) (col : int array) (n : int) : int array =
  raise NotImplemented

(* TODO: Implement is_valid_board *)
let full_valid_board (board : int array array) (n : int) : bool =
  raise NotImplemented



(*** QUESTION 2: BRUTE FORCE SOLVER ***)

(* TODO: Implement row_valid *)
let row_valid (board : int array array) (row : int array) (value : int) : bool =
  raise NotImplemented

(* TODO: Implement col_valid *)
let col_valid (board : int array array) (col : int array) (value : int) : bool =
  raise NotImplemented

(* TODO: Implement square_valid *)
let square_valid (board : int array array) (row : int array) (col : int array) (value : int) (n : int) : bool =
  raise NotImplemented

(* TODO: Implement board_valid *)
let check_board_valid (board : int array array) (row : int array) (col : int array) (value : int) (n : int) : bool =
  raise NotImplemented

(* TODO: Implement find_empty *)
let find_empty (board : int array array) (n : int) : (int * int) option =
  raise NotImplemented

(* TODO: Implement solve *)
let rec solve (board : int array array) (n : int) : (int array array) option =
  raise NotImplemented



(*** QUESTION 3: CONSTRAINT PROBLEM (ninja level) ***)

(* TODO: Implement create_sudoku_grid *)
let create_sudoku_grid ctx m =
  raise NotImplemented

(* TODO: Implement add_digit_constraints *)
let add_digit_constraints ctx m grid solver =
  raise NotImplemented

(* TODO: Implement add_distinct_row_constraint *)
let add_distinct_row_constraint ctx m grid solver =
  raise NotImplemented

(* TODO: Implement add_distinct_col_constraint *)
let add_distinct_col_constraint ctx m grid solver =
  raise NotImplemented

(* TODO: Implement add_distinct_square_constraint *)
let add_distinct_square_constraint ctx m grid solver =
  raise NotImplemented

(* TODO: Implement add_instance_constraints *)
let add_instance_constraints ctx grid solver instance = 
  raise NotImplemented

(* TODO: Implement solve_sudoku. You may find create_solver and print_solution useful *)
let solve_sudoku ctx m instance =
  raise NotImplemented
  
