#**Sudoku-Solver**

Sudoku is a single-player game played on a mxm board (divided into m squares), with the goal of filling the board with digits from 1 to m inclusive. Normally, the board will have a small amount of cells prefilled, and it’s the players job to abide by 3 properties in order to legally fill the rest of it.
The properties are:
1. All values in a row must be unique
2. All values in a column must be unique
3. All values in a square must be unique

Your job is to make a couple OCaml functions that can solve a sudoku board for us.
**THINGS TO NOTE:**
n is the side length of a square in the board, thus m = n*n, if the board is mxm
Helper code given to you contains the board type, a function that checks if an array contains all the integers uniquely from 1 to m, and a function that prints a board

##**PART 1: Validator**
1. Before we jump in, let’s make something that’ll check our answers.
2. get_row: returns the row’th row in an array
3. get_column: returns the col’th column in an array
4. get_square: returns the square that the value in (row, col) belongs to in an array
5. Full_valid_board: given a completed board, call the helper functions you previously made to check if the board provided abides by all the properties

# **PART 2: Brute force** 
A popular programming problem is the brute force sudoku problem. It uses recursive backtracking to “try” every possible value in a cell, descending into the decision tree with each attempted value, then once it hits a dead end (meaning there are no possible values for a cell) it backtracks, stepping back into the previous cell, incrementing its “attempted value”, and attempting to continue forward once again. This continues until either the board is filled (solution found), or the empty cell the function started with (typically 0,0) has run out of options (no solution exists).
1. Row_valid: checks that if value “value” was added to the inputted row, would that row still abide by the row property?
2. Col_valid: checks that if value “value” was added to the inputted column, would that column still abide by the column property?
3. Square_valid: checks that if value “value” was added to the square the cell (row, col) belongs to, would that square still abide by the square property?
4. Check_board_valid: checks that if value “value” was added to the board in position (row, col), would the board abide by the sudoku properties?
5. Find_empty: given a board, finds the next empty cell 
    - Checks left-to-right top-to-bottom
    - Empty cells are filled with the value “0”
6. Solve: put all the functions together with some logic to solve the full sudoku board


##**PART 3: Faster algorithm**
Unfortunately that, while popular, the brute force algorithm is terribly slow, as in it can be O(m^m)???, which is not good. So we want to make a faster one.
