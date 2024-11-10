
(* sqrt with integer *)
let isqrt (n : int) = 
  int_of_float (sqrt (float_of_int n))

(* checks if the inputted list matches a sorted list from 1 to n *)
let is_valid_list (lst : int list) (n : int) =
  let sorted_lst = List.sort compare lst in
  sorted_lst = List.init n (fun i -> i + 1)

(* grabs the rowth row from the board *)
let get_row (b : int list list) (row : int) =
  List.nth b row

(* grabs the colth column from the board *)
let get_column (b : int list list) (n : int) (col : int) =
  List.init n (fun i -> List.nth (List.nth b i) col)

(* grabs the subgrid that the value at (row, col) belongs to from the board *)
let get_subgrid (b : int list list) (m : int) (row : int) (col : int) =
  (* m has been preprocessed  to be the subgrid size *)
  let row_start = row - (row mod m) in
  let col_start = col - (col mod m) in

  (* loops through subgrid, adding elements to a list *)
  let rec collect_subgrid i j acc =
    if i >= row_start + m then acc (* out of rows *)
    else if j >= col_start + m then collect_subgrid (i + 1) col_start acc (* out of cols in row *)
    else collect_subgrid i (j + 1) (List.nth (List.nth b i) j :: acc)
  in
  collect_subgrid row_start col_start []

(* checks to see if the filled board is a valid board - as in satisfies the constraints *)
let is_valid_board (b : int list list) (n: int) =
  (* n is the side length of the board *)
  let subgrid_size = isqrt(n) in (* length of a subgrid in the board *)

  let valid_rows = List.for_all (fun row -> is_valid_list (get_row b row) n) (List.init n (fun x -> x)) in

  let valid_columns = List.for_all (fun col -> is_valid_list (get_column b n col) n) (List.init n (fun x -> x)) in

  let valid_subgrids =
    let indices = List.init (n / subgrid_size) (fun i -> i * subgrid_size) in (* makes list of 0 to n-1 (inclusive) stepped up by block_size*)
    List.for_all (fun row ->
      List.for_all (fun col ->
        is_valid_list (get_subgrid b subgrid_size row col) n
      ) indices
    ) indices
  in
  valid_rows && valid_columns && valid_subgrids



(*
let example_board = [
  [5; 3; 4; 6; 7; 8; 9; 1; 2];
  [6; 7; 2; 1; 9; 5; 3; 4; 8];
  [1; 9; 8; 3; 4; 2; 5; 6; 7];
  [8; 5; 9; 7; 6; 1; 4; 2; 3];
  [4; 2; 6; 8; 5; 3; 7; 9; 1];
  [7; 1; 3; 9; 2; 4; 8; 5; 6];
  [9; 6; 1; 5; 3; 7; 2; 8; 4];
  [2; 8; 7; 4; 1; 9; 6; 3; 5];
  [3; 4; 5; 2; 8; 6; 1; 7; 9]
]

let () =
  if is_valid_board example_board (List.length example_board) then
    print_endline "Valid"
  else
    print_endline "Invalid"
*)
    
    

      