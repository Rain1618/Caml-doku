(* !! n is the side length of the squares, 0 resemble empty cells *)


type board = int array array


(* checks if value can legally be added to the row *)
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


 (* prints board *)
let print_board board =
 Array.iter (fun row ->
   Array.iter (fun cell ->
     Printf.printf "%d " cell
   ) row;
   print_newline ()
 ) board




(* example with 9x9 board *)
let () =
let board = [|
 [|5; 3; 0; 0; 7; 0; 0; 0; 0|];
 [|6; 0; 0; 1; 9; 5; 0; 0; 0|];
 [|0; 9; 8; 0; 0; 0; 0; 6; 0|];
 [|8; 0; 0; 0; 6; 0; 0; 0; 3|];
 [|4; 0; 0; 8; 0; 3; 0; 0; 1|];
 [|7; 0; 0; 0; 2; 0; 0; 0; 6|];
 [|0; 6; 0; 0; 0; 0; 2; 8; 0|];
 [|0; 0; 0; 4; 1; 9; 0; 0; 5|];
 [|0; 0; 0; 0; 8; 0; 0; 7; 9|];
|] in
let n = 3 in
 match solve board n with
 | Some solved_board -> print_board solved_board
 | None -> Printf.printf "No solution exists\n"
