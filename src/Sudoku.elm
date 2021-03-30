module Sudoku exposing (Board, Cell, Position, get_cell, set_cell, empty_board, remove_cells, board_solved, ordered_pos)

import List.Extra exposing (getAt, updateAt)
import List exposing (repeat, member, all, reverse)

-----------------------------------------

{-- Board as a List of Lists of Cells --}

{-- A Cell can take Char values from '1' to '9' --}
{-- ('0' if not filled). --}
type alias Cell = Char 
{-- A Board is a list of Cells. We'll call it a 'row-based' representation.--}
type alias Board = List (List Cell)
{-- A Position indicates (row, column) of a Board. --}
{-- row and column are integers between 0 and 8. --}
type alias Position = ( Int, Int )

{-- Returns a value at Position of a particular nested List. --}
{-- Returns Nothing if the Position is out of range. --}
get_cell_helper : Position -> (List (List a)) -> Maybe a
get_cell_helper ( row, col ) li = 
  let
    list_r = getAt row li 
  in
    case list_r of
      Nothing ->
        Nothing
      Just l -> 
        getAt col l

{-- Returns the cell value at a given Position with certainty. --}
{-- Returns an error message if the index is out of range. --}
get_cell : Position -> (List (List a)) -> a
get_cell ( row, col ) li = 
  let 
    val = get_cell_helper ( row, col ) li
  in
    case val of 
      Just x -> 
        x
      Nothing ->
        Debug.todo "get_cell_value: Index out of range!"

{-- Updates a cell of a Board at a given position and returns a new Board. --}
{-- Returns the original Board if the Position is out of range. --}
set_cell : Position -> a -> (List (List a)) -> (List (List a)) 
set_cell ( row, col ) val li = 
  let 
    list_r = getAt row li
  in
    case list_r of 
      Nothing ->
        li
      Just l ->
        let
          new_r = updateAt col (\_ -> val) l
        in
          updateAt row (\_ -> new_r) li

{-- Returns an empty Board. --}
empty_board : Board
empty_board = 
  let 
    rows = repeat 9 '0'
  in 
    repeat 9 rows 

{-- Eliminates a value from a Cell at a given Position. --}
{-- Returns the updated Board with 0 at the Position. --}
eliminate : Position -> Board -> Board
eliminate pos board = 
  set_cell pos '0' board

{-- Sets the cells at given positions to 0. --}
{-- Returns a new Board with modified cells. --}
remove_cells : (List Position) -> Board -> Board
remove_cells positions board = 
  case positions of 
    [] -> 
      board
    first::rest ->
      remove_cells rest (eliminate first board) 

{-- Winning Conditions. --}
{-- Check if all elements in a list are unique and are between '1' and '9' --}
{-- assuming the input list is a list of Chars with length 9. --}
members_unique : (List Cell) -> Bool
members_unique list = 
  members_unique_helper [] list

members_unique_helper : (List Cell) -> (List Cell) -> Bool
members_unique_helper li list = 
  case list of
    c::rest ->
      if ('1' <= c) && (c <= '9') then
        if (member c li) == True then
          False
        else
          members_unique_helper (c::li) rest
      else
        False
    [] ->
      True 

{-- Check if all given lists of Cells satisfy the members_unique condition, --}
{-- assuming each list is a list of Chars with length 9. --}
lists_unique : (List (List Cell)) -> Bool
lists_unique lists = 
  case lists of 
    li::rest ->
      let b = members_unique li in
        if b == True then lists_unique rest
        else False
    [] ->
      True

{-- Returns a list with nth values taken from each list given a list of lists --}
{-- (n starting from 0). --}
nth_values : Int -> (List (List a)) -> (List a)
nth_values n lists = 
  nth_values_helper n lists []

nth_values_helper : Int -> (List (List a)) -> (List a) -> (List a)
nth_values_helper n lists output =
  case lists of 
    li::rest ->
      let nth = getAt n li in
        case nth of 
          Nothing -> 
            Debug.todo "nth_values_helper: index out of range!"
          Just x -> 
            nth_values_helper n rest (output++[x])
    [] ->
      output

{-- Convert a Board into a 'column-based' representation, (rotates it by 90 degrees) --}
{-- assuming it is a proper 9x9 Board with values from '0' to '9'. --}
rotate_board : Board -> Board
rotate_board board = 
  let 
    c0 = nth_values 0 board
    c1 = nth_values 1 board
    c2 = nth_values 2 board
    c3 = nth_values 3 board
    c4 = nth_values 4 board
    c5 = nth_values 5 board
    c6 = nth_values 6 board
    c7 = nth_values 7 board
    c8 = nth_values 8 board
  in 
    [c0, c1, c2, c3, c4, c5, c6, c7, c8]

{-- Returns a list of cells in a 3x3 grid given a board, --}
{-- and the position of the top left cell. --}
get_grid_cells : Position -> Board -> (List Cell)
get_grid_cells ( row, col ) board = 
  let
    v1 = get_cell ( row, col ) board
    v2 = get_cell ( row, col + 1 ) board
    v3 = get_cell ( row, col + 2) board
    v4 = get_cell ( row + 1, col ) board
    v5 = get_cell ( row + 1, col + 1 ) board
    v6 = get_cell ( row + 1, col + 2 ) board
    v7 = get_cell ( row + 2, col ) board
    v8 = get_cell ( row + 2, col + 1 ) board
    v9 = get_cell ( row + 2, col + 2 ) board
  in
    [v1, v2, v3, v4, v5, v6, v7, v8, v9]

{-- Returns the list of 3x3 grids in a given board. --}
{-- Assumes the Board is a proper 9x9 Sudoku Board. --}
get_grids : Board -> List (List Cell)
get_grids board = 
  let 
    g1 = get_grid_cells ( 0, 0 ) board
    g2 = get_grid_cells ( 0, 3 ) board
    g3 = get_grid_cells ( 0, 6 ) board
    g4 = get_grid_cells ( 3, 0 ) board
    g5 = get_grid_cells ( 3, 3 ) board
    g6 = get_grid_cells ( 3, 6 ) board
    g7 = get_grid_cells ( 6, 0 ) board
    g8 = get_grid_cells ( 6, 3 ) board
    g9 = get_grid_cells ( 6, 6 ) board
  in
    [g1, g2, g3, g4, g5, g6, g7, g8, g9]

{-- Checks if a given Board is solved. --}
{-- Assumes the Board is a propuer 9x9 Sudoku Board. --}
board_solved : Board -> Bool
board_solved board = 
  let 
    col_board = rotate_board board
    grids = get_grids board
  in
    (lists_unique board) && (lists_unique col_board) && (lists_unique grids)

ordered_row : Int -> (List Position)
ordered_row i = 
  [ (i, 0), (i, 1), (i, 2), 
    (i, 3), (i, 4), (i, 5), 
    (i, 6), (i, 7), (i, 8) ]

-- List of all possible positions in an ordered manner
ordered_pos : (List Position)
ordered_pos = 
  let
    r1 = ordered_row 0
    r2 = ordered_row 1
    r3 = ordered_row 2
    r4 = ordered_row 3
    r5 = ordered_row 4
    r6 = ordered_row 5
    r7 = ordered_row 6
    r8 = ordered_row 7
    r9 = ordered_row 8

  in
    (r1 ++ r2 ++ r3 ++ r4 ++ r5 ++ r6 ++ r7 ++ r8 ++ r9)
  