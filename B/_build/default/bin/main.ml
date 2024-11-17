(* open Z3
open Z3.Solver idk why we dont need the imports lol*)

(*Refer to the following link for a description of all the Z3 functions used*)
(*https://z3prover.github.io/api/html/ml/Z3.Z3Array.html*)

(*Our sudoku representation: 
   Each sudoku is a m by m GRID
   There are m n by n SQUARES in each grid
   There are m*m CELLS in each grid
   where n is any natural number > 0
   0 represents an empty cell --> no value assigned yet*)

let create_sudoku_grid ctx m =
  (* rows *)
  Array.init m (fun i -> 
      (* cols *)
      Array.init m (fun j -> 
          Z3.Arithmetic.Integer.mk_const_s ctx
            (Printf.sprintf "x_%d_%d" (i + 1) (j + 1))));;

(*TODO: string or integer representation?? choose which one is better, i did string rn butwe can change*)
(*CONSTRAINT: for each cell, the digit i must satisfy 1<= i <= m *)
let add_digit_constraints ctx m grid solver =
  let int_one = Z3.Arithmetic.Integer.mk_numeral_s ctx "1" in
  let int_m = Z3.Arithmetic.Integer.mk_numeral_s ctx (string_of_int m) in
  Array.iteri (fun _ row ->
      Array.iteri (fun _ cell ->
          let lower_bound = Z3.Arithmetic.mk_ge ctx cell int_one in (*mk_ge : context -> Expr.expr -> Expr.expr -> Expr.expr Create an expression representing t1 >= t2*)
          let upper_bound = Z3.Arithmetic.mk_le ctx cell int_m in
          let constraint_ = Z3.Boolean.mk_and ctx [lower_bound; upper_bound] in (* mk_and : context -> Expr.expr list -> Expr.expr*)
          Z3.Solver.add solver [constraint_]
      ) row
  ) grid;;

(*TODO: IS there clever way of using the same code for the following 3 constraints? maybe have one function that gets makes a list for each row/col/square and then reuse same mk_distinct function on that list?*)
(*CONSTRAINT: Every element in a row must be distinct*)
let add_distinct_row_constraint ctx m grid solver =
  for i = 0 to m - 1 do
    let row = Array.to_list grid.(i) in 
    let constraint_ = Z3.Boolean.mk_distinct ctx row in
    Z3.Solver.add solver [constraint_]
  done

(*CONSTRAINT: Every element in a column must be distinct*)
let add_distinct_col_constraint ctx m grid solver = 
  for j = 0 to m - 1 do
    let column = Array.init m (fun i -> grid.(i).(j)) in
    let constraint_ = Z3.Boolean.mk_distinct ctx (Array.to_list column) in
    Z3.Solver.add solver [constraint_]
  done

(*CONSTRAINT: Every element in a nxn square must be distinct*)
let add_distinct_square_constraint ctx m grid solver =
  let n = int_of_float (sqrt (float_of_int m)) in
  for i0 = 0 to n - 1 do (*TODO: is a for loop the most ocamlian way of doing things?*)
    for j0 = 0 to n - 1 do
      (* Collect all elements in square starting at (n * i0, n * j0) *)
      let square =
        List.flatten (List.init n (fun i ->
          List.init n (fun j ->
            grid.(n * i0 + i).(n * j0 + j)
          )
        ))
      in
      let constraint_ = Z3.Boolean.mk_distinct ctx square in
      Z3.Solver.add solver [constraint_]
    done
  done

(* CONSTRAINT: For a given instance of a sudoku, there are fixed values that must be respected (duh)*)
let add_instance_constraints ctx grid solver instance =
  Array.iteri (fun i row ->
      Array.iteri (fun j value ->
          if value <> 0 then (*value != 0 goofy ocaml syntax*)
            let cell_value = Z3.Arithmetic.Integer.mk_numeral_s ctx (string_of_int value) in
            let constraint_ = Z3.Boolean.mk_eq ctx grid.(i).(j) cell_value in
            Z3.Solver.add solver [constraint_]
      ) row
  ) instance;;

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

(* Main*)
let solve_sudoku ctx m instance =
  let grid = create_sudoku_grid ctx m in
  let solver = create_solver ctx in
  add_digit_constraints ctx m grid solver;
  add_instance_constraints ctx grid solver instance;
  add_distinct_row_constraint ctx m grid solver;
  add_distinct_col_constraint ctx m grid solver;
  add_distinct_square_constraint ctx m grid solver;
  
  match Z3.Solver.check solver [] with
  | Z3.Solver.SATISFIABLE ->
      let model_option = Z3.Solver.get_model solver in
      (match model_option with
      | Some model -> 
          print_solution grid model  
      | None -> print_endline "No solution found in model")
  | Z3.Solver.UNSATISFIABLE -> print_endline "No solution"
  | Z3.Solver.UNKNOWN -> print_endline "idk what unknown means lol"

(* Test *)
(*TODO: find some famously hard sudokus and try them out*)
let m = 4;;

let instance = [|
  [|0;0;0;4|];
  [|0;3;0;0|];
  [|0;0;4;0|];
  [|2;0;0;0|];
|]

let () =
  let ctx = Z3.mk_context [] in
  solve_sudoku ctx m instance