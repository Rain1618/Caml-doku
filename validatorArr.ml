(* !! n is the side length of the squares, 0 resemble empty cells *)

type board = int array array

let is_valid_list arr n =
  let arr_copy = Array.copy arr in  (* Create a copy of the array *)
  Array.sort compare arr_copy;  (* Sort the copy in-place *)
  arr_copy = Array.init (n * n) (fun i -> i + 1)  (* Compare sorted array with expected array *)

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
  


let () =
  let board = [|
    [|5; 3; 4; 6; 7; 8; 9; 1; 2|];
    [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
    [|1; 9; 8; 3; 4; 2; 5; 6; 7|];
    [|8; 5; 9; 7; 6; 1; 4; 2; 3|];
    [|4; 2; 6; 8; 5; 3; 7; 9; 1|];
    [|7; 1; 3; 9; 2; 4; 8; 5; 6|];
    [|9; 6; 1; 5; 3; 7; 2; 8; 4|];
    [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
    [|3; 4; 5; 2; 8; 6; 1; 7; 9|];
  |] in
  let n = 3 in
  match full_valid_board board n with
  | true -> print_endline "Valid"
  | false -> print_endline "Invalid"