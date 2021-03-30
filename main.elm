module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Attributes as Attr
import List exposing (member, take, drop)
import Random exposing (Generator, initialSeed, generate)
import Random.List exposing (shuffle)
import Sudoku exposing (Board, Cell, Position, get_cell, set_cell, empty_board, remove_cells, board_solved, ordered_pos)

-- MAIN 

main = 
  Browser.element 
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view }

-- MODEL

type alias Model = 
  { board : Board
  , puzzle_cells : (List Position)
  , solution : Board
  , won : Bool
  } 

-- MSG

type Msg = Place Cell Position | Solve 
    | Reset | NewRow (List Char) 
    | NewPositions (List Position)

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of 
    Place cell pos -> 
      let 
        b_new = set_cell pos cell model.board
        solved = board_solved b_new 
      in
        if solved == True then ( { model | board = b_new, puzzle_cells = ordered_pos, won = True }
                               , Cmd.none )
        else ( { model | board = b_new }, Cmd.none )
    Solve -> 
      ( { model | board = model.solution, puzzle_cells = ordered_pos }, Cmd.none )
    Reset -> 
      ( model
      , generate NewRow row_gen )
    NewRow r ->
      let rows = ordered_rows r in 
        ( { model | solution = rows }
        , generate NewPositions (shuffle ordered_pos) )
    NewPositions pos -> 
      let 
        empty_pos = (take 60 pos)
        new_board = (remove_cells empty_pos model.solution)
        _ = Debug.log "solution" model.solution
        given = (drop 60 pos)
      in 
        ( { model | board = new_board, puzzle_cells = given, won = False } 
        , Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- INIT MODEL

init : () -> ( Model, Cmd Msg )
init _ =
  ( { board = empty_board
  , puzzle_cells = ordered_pos
  , solution = empty_board
  , won = False }
  , Cmd.none )

-- VIEW

view : Model -> Html Msg 
view model = 
  let 
    solve_styles = [("float", "left"), ("font-family", "Helvetica"), 
                    ("font-weight", "bold"), ("border-radius", "12px"),
                    ("color", "white"), ("font-size", "1em"), 
                    ("background-color", "red"), ("margin", "5px")]
    reset_styles = [("float", "left"), ("font-family", "Helvetica"), 
                    ("font-weight", "bold"), ("border-radius", "12px"),
                    ("color", "white"), ("font-size", "1em"), 
                    ("background-color", "blue"), ("margin", "5px")]
    msg_styles = [  ("text-align", "center"), ("font-family", "Helvetica"), 
                    ("font-weight", "bold"), ("font-size", "1em"), 
                    ("font-color", "green"), ("margin", "5px")]
    solve_button = Html.button ((onClick Solve)::(List.map map_style solve_styles)) [Html.text "Solve It!"]
    reset_button = Html.button ((onClick Reset)::(List.map map_style reset_styles)) [Html.text "New Puzzle"]
    won_message = Html.div (List.map map_style msg_styles) [Html.text "Congratulations! You've won!"]
    header_styles = [("color", "#800000"), ("text-align", "center"), ("font-weight", "bold")]
    div_styles = [("display", "flex"), ("justify-content", "center")]
    buttons = Html.div (List.map map_style div_styles) [ reset_button, solve_button ]
    header = Html.h1 (List.map map_style header_styles) [Html.text "Sudoku Board Game"]
  in
    if model.won == False then
      Html.div [] [ header, buttons, game_board model.puzzle_cells model.board ]
    else
      Html.div [] [ header, buttons, won_message, game_board model.puzzle_cells model.board ]


-----------------------------------------

-- UI

-- Helper function for mapping CSS elements
map_style : (String, String) -> Html.Attribute Msg 
map_style (k, v) = 
  Attr.style k v 

-- Individual choice button
-- Chooses a number and 'places' it in the Board list
-- Undoes the choice when clicked again on a number
cell_choice : Cell -> Bool -> Position -> Html Msg 
cell_choice cell chosen pos = 
  let 
    styles_default = 
      [ ("font-weight", "bold")
      , ("font-size", "8px")
      , ("text-align", "center")
      , ("display", "flex")
      , ("border-radius", "50%")
      , ("background-color", "rgb(220, 220, 220)")
      ]
    styles_chosen = 
      [ ("font-weight", "bold")
      , ("font-size", "8px")
      , ("text-align", "center")
      , ("display", "flex")
      , ("border-radius", "50%")
      , ("background-color", "rgb(0, 255, 255)")
      ]
  in 
    if chosen == True then
      Html.button ((onClick (Place '0' pos))::(List.map map_style styles_chosen)) [ (Html.text (String.fromChar cell)) ]
    else
      Html.button ((onClick (Place cell pos))::(List.map map_style styles_default)) [ (Html.text (String.fromChar cell)) ]

-- Draws a list of buttons in a single cell
generate_choices : Cell -> Position -> List (Html Msg)
generate_choices chosen_c pos = 
  let 
    c1 = ((cell_choice '1' False pos), '1')
    c2 = ((cell_choice '2' False pos), '2')
    c3 = ((cell_choice '3' False pos), '3')
    c4 = ((cell_choice '4' False pos), '4')
    c5 = ((cell_choice '5' False pos), '5')
    c6 = ((cell_choice '6' False pos), '6')
    c7 = ((cell_choice '7' False pos), '7')
    c8 = ((cell_choice '8' False pos), '8')
    c9 = ((cell_choice '9' False pos), '9')
    c = [c1, c2, c3, c4, c5, c6, c7, c8, c9]
    cs = [ (Tuple.first c1)
         , (Tuple.first c2)
         , (Tuple.first c3)
         , (Tuple.first c4)
         , (Tuple.first c5)
         , (Tuple.first c6)
         , (Tuple.first c7)
         , (Tuple.first c8)
         , (Tuple.first c9)
         ]
  in
    if chosen_c == '0' then 
      cs 
    else if ('1' <= chosen_c) && (chosen_c <= '9') then
      List.map (\(a, b) -> if b == chosen_c then (cell_choice b True pos) else a) c
    else
      Debug.todo "generate_choices: invalid cell choice!"

-- A single box/cell
-- Takes the position of the cell, given puzzle cell positions, and the entire board
-- and produces an HTML cell box.
box : Position -> (List Position) -> Board -> Html Msg 
box pos puzzle board = 
  let 
    box_styles_grid = 
      [ ("border", "3px solid #2c3e50")
      , ("border-radius", "3px")
      , ("font-family", "Helvetica")
      , ("font-weight", "bold")
      , ("font-size", "3em")
      , ("display", "grid")
      , ("grid-template", "repeat(3, 1fr) / repeat(3, 1fr)")
      , ("justify-content", "center")
      , ("align-items", "center")
      ]
    box_styles_default = 
      [ ("border", "3px solid #2c3e50")
      , ("border-radius", "3px")
      , ("font-family", "Helvetica")
      , ("font-weight", "bold")
      , ("font-size", "3em")
      , ("display", "grid")
      , ("justify-content", "center")
      , ("align-items", "center")
      ]
    cell_val = get_cell pos board
  in 
    if (member pos puzzle) == True then
      Html.div (List.map map_style box_styles_default) 
              [(cell_val |> String.fromChar |> Html.text)]
    else 
      Html.div (List.map map_style box_styles_grid)
              (generate_choices cell_val pos)

-- Game Board
game_board : (List Position) -> Board -> Html Msg 
game_board puzzle board = 
  let 
    board_styles = 
      [ ("width", "630px")
      , ("height", "630px")
      , ("margin", "0 auto")
      , ("background-color", "#34495e")
      , ("color", "#fff")
      , ("border", "3px solid #2c3e50")
      , ("border-radius", "5px")
      , ("display", "grid")
      , ("grid-template", "repeat(9, 1fr) / repeat(9, 1fr)")
      ]
    boxes = List.map (\p -> (box p puzzle board)) ordered_pos
  in
    Html.div (List.map map_style board_styles) boxes

-----------------------------------------

-- PUZZLE GENERATOR

-- shuffle a single list of Chars
row_gen : Generator (List Char)
row_gen = 
  shuffle ['1', '2', '3', '4', '5', '6', '7', '8', '9']

-- Helper function: shifts a list to the left by n. 
shiftLeft : (List a) -> Int -> (List a)
shiftLeft l n = 
  if n <= 0 then l
  else
    case l of 
      first::rest -> 
        let new_l = rest ++ [first] in
          shiftLeft new_l (n-1)
      _ -> Debug.todo "shiftLeft: can't shift an empty list!"

-- Returns ordered rows such that each row is shifted version of its previous row.
ordered_rows : List Char -> List (List Char)
ordered_rows r = 
  let 
    r2 = shiftLeft r 3
    r3 = shiftLeft r2 3
    r4 = shiftLeft r3 1
    r5 = shiftLeft r4 3
    r6 = shiftLeft r5 3
    r7 = shiftLeft r6 1
    r8 = shiftLeft r7 3
    r9 = shiftLeft r8 3
  in
    [r, r2, r3, r4, r5, r6, r7, r8, r9]
