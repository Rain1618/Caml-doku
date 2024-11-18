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

(* grabs the rowth row from the board *)
let get_row board row =
  Array.get board row

(* grabs the colth column from the board *)
let get_column board col n =
  Array.init (n*n) (fun i -> Array.get (Array.get board i) col)

(* grabs the subgrid that the value at (row, col) belongs to from the board *)
let get_square board row col n =
  let row_start = (row / n) * n in
  let col_start = (col / n) * n in
  let arr = Array.init (n * n) (fun _ -> 0) in

  (* loops through square, adding elements to the square array *)
  let rec collect_square row_i col_i cur =
    if row_i >= row_start + n then () (* out of rows *)
    else if col_i >= col_start + n then collect_square (row_i + 1) col_start cur (* out of cols in row, move down *)
    else (
      arr.(cur) <- Array.get (Array.get board row_i) col_i; (* store value from cell in arr *)
      collect_square row_i (col_i + 1) (cur + 1) (* step forward *)
    )
  in
  collect_square row_start col_start 0; arr

(* uses the row, column, and square getters to ensure properties hold for all of them *)
let full_valid_board board n =
  let valid_rows = Array.for_all (fun row -> is_valid_list (get_row board row) n) (Array.init (n*n) (fun x -> x)) in
  let valid_columns = Array.for_all (fun col -> is_valid_list (get_column board col n) n) (Array.init (n*n) (fun x -> x)) in
  let valid_squares =
    Array.for_all (fun row ->
      Array.for_all (fun col ->
        is_valid_list (get_square board row col n) n
      ) (Array.init n (fun i -> i))
    ) (Array.init n (fun i -> i))
  in
  valid_rows && valid_columns && valid_squares


(*** QUESTION 2: BRUTE FORCE SOLVER ***)


let row_valid board row value =
  not (Array.exists ((=) value) board.(row))

(* checks if value can legally be added to the column *)
let col_valid board col value =
  not (Array.exists (fun row -> row.(col) = value) board)

(* checks if value can legally be added to the square its in *)
let square_valid board row col value n =
  let start_row = (row / n) * n in
  let start_col = (col / n) * n in
  let rec check r c = match r, c with
    | r, c when r = start_row + n -> true
    | r, c when c = start_col + n -> check (r+1) start_col
    | r, c when board.(r).(c) = value -> false
    | r, c -> check r (c+1)
  in
  check start_row start_col

(* checks if the added value follows board rules *)
let check_board_valid board row col value n =
  row_valid board row value &&
  col_valid board col value &&
  square_valid board row col value n

(* finds next empty cell *)
let find_empty board n =
  let rec search r c = match (r, c) with
    | r,c when r = n*n -> None (* reached end of cols *)
    | r,c when c = n*n -> search (r+1) 0 (* reached end of row, move to next *)
    | r,c when board.(r).(c) = 0 -> Some (r, c) (* found empty *)
    | r,c -> search r (c + 1) (* move to next cell *)
  in
  search 0 0

(* checks if value can legally be added to the row *)
let rec solve board n = match find_empty board n with
  | None -> Some board  (* no empties, so board is solved *)
  | Some (row, col) ->
      let rec try_value value =
        if value > n*n then None  (* no valid options, backtrack *)
        else if check_board_valid board row col value n then (
          board.(row).(col) <- value;
          match solve board n with
          | Some solution -> Some solution  (* solution found *)
          | None ->
              board.(row).(col) <- 0;  (* undo, try next value *)
              try_value (value + 1)
        ) else try_value (value + 1)
      in try_value 1

