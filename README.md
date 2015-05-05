# Sudoku Solver

A quick attempt at writing a sudoku solver.
I make no guarantees as to the quality or correctness of this code.
It works on a single test case I tried.

## Idea

1. If the board is impossible or has been solved, stop.
2. In each cell, write down all the possible values that could go there.
   This is the set of values not appearing in the row, column, or "box"¹ for
   that cell.
3. The most constrained cell is the one with fewest possibilities.
   Starting with the most constrained, try to fix each of its values,
   and repeat the algorithm with each of these possibilities until 
   a solution is found.

¹ a box is one of the nine 3x3 squares making up the sudoku grid.
  The box that a cell belongs to is the one it's inside.

## Data representation

Cell values are represented as lists of possible values.

* An empty list in any cell means the whole board is impossible.
* A singleton list means the cell value has been "fixed", and now cannot
  change.
