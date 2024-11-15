(* open Z3
open Z3.Solver*)

let n = 3  
let m = n*n (*Sudoku size  *)

(*https://z3prover.github.io/api/html/ml/Z3.Z3Array.html*)

let create_sudoku_grid ctx m =
  Array.init m (fun i -> (* rows *)
      Array.init m (fun j -> (* columns *)
          Z3.Arithmetic.Integer.mk_const_s ctx
            (Printf.sprintf "x_%d_%d" (i + 1) (j + 1))));;

(*Print out grid*)
(* let ctx = Z3.mk_context [] in
let grid = create_sudoku_grid ctx m in
(* Print variable names *)
Array.iter (fun row ->
    Array.iter (fun cell -> Printf.printf "%s " (Z3.Expr.to_string cell)) row;
    print_newline ())
  grid  *)

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

(*CONSTRAINT:*)

(* let ctx = Z3.mk_context [] in
let grid = create_sudoku_grid ctx m in
let solver = Z3.Solver.mk_solver ctx None in

add_digit_constraints ctx m grid solver 

let result = Z3.Solver.check solver [] in
match result with
| Z3.Solver.SATISFIABLE -> print_endline "SATISFIABLE"
| Z3.Solver.UNSATISFIABLE -> print_endline "UNSATISFIABLE"
| Z3.Solver.UNKNOWN -> print_endline "UNKNOWN" *)

(* Create a solver *)
let create_solver ctx =
  let solver = Z3.Solver.mk_solver ctx None in
  solver

(* Print the solution *)
let print_solution ctx grid model =
  Array.iteri (fun i row ->
    Array.iteri (fun j cell ->
        (* Evaluate the value of each cell in the solution *)
        let eval_result = Z3.Model.eval model cell true in
        match eval_result with
        | Some expr -> 
            (* Extract the integer value as a string *)
            let value_str = Z3.Expr.to_string expr in
            Printf.printf "Cell (%d, %d): %s\n" (i + 1) (j + 1) value_str
        | None -> Printf.printf "Cell (%d, %d): Unassigned\n" (i + 1) (j + 1)
    ) row
) grid

(* Main function to create, add constraints, and solve *)
let solve_sudoku ctx m instance =
  let grid = create_sudoku_grid ctx m in
  let solver = create_solver ctx in
  add_digit_constraints ctx m grid solver;
  (* add_distinct_constraints ctx grid solver;
  add_instance_constraints ctx grid solver; *)
  
  match Z3.Solver.check solver [] with
  | Z3.Solver.SATISFIABLE ->
      let model_option = Z3.Solver.get_model solver in
      (match model_option with
      | Some model -> 
          print_solution ctx grid model  (* If model is found, print the solution *)
      | None -> print_endline "No solution found in model")
  | Z3.Solver.UNSATISFIABLE -> print_endline "No solution"
  | Z3.Solver.UNKNOWN -> print_endline "Unknown result"

(* Define a sample 2x2 Sudoku instance *)
let instance = [|
  [|5; 3; 0; 0; 7; 0; 0; 0; 0|];
  [|6; 0; 0; 1; 9; 5; 0; 0; 0|];
  [|0; 9; 8; 0; 0; 0; 0; 6; 0|];
  [|8; 0; 0; 0; 6; 0; 0; 0; 3|];
  [|4; 0; 0; 8; 0; 3; 0; 0; 1|];
  [|7; 0; 0; 0; 2; 0; 0; 0; 6|];
  [|0; 6; 0; 0; 0; 0; 2; 8; 0|];
  [|0; 0; 0; 4; 1; 9; 0; 0; 5|];
  [|0; 0; 0; 0; 8; 0; 0; 7; 9|]
|]

(* Run the Sudoku solver for a 9x9 grid with m=9 *)
let () =
  let ctx = Z3.mk_context [] in
  solve_sudoku ctx m instance