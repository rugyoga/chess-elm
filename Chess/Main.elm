import Array exposing (get)
import Char
import Color
import Dict exposing (Dict, insert)
import Graphics.Element exposing (Element, above, below, beside, centered, color, container, down, empty, flow, leftAligned, middle, opacity, outward, right, size, spacer, width)
import Graphics.Input exposing (button, clickable, dropDown)
import Html exposing (Html, fromElement)
import List exposing (filter, map, map2, member)
import Maybe exposing (withDefault)
import StartApp.Simple exposing (start)
import String exposing (toLower)
import Text exposing (fromString, height, monospace, typeface)

import Chess.Chess exposing (Board, Color(..), ColorPiece, Game, Info(..), Move, Piece(..), SquareIndex, board_clear, board_get, board_set, game_to_FEN, initial_game, legal_moves, make_move, move_to_string, opposite, rank_index_to_string, square_to_string)

square n = size n n empty
unit = 60
unit_square = square unit
unit_spacer = spacer unit unit
unit_string = fromString >> typeface ["arial", "tahoma", "helvetica"] >> height (0.75 * unit) >> centered >> container unit unit middle
white_square = color Color.lightGrey unit_square
black_square = color Color.darkGrey unit_square
legend s = unit_square

alternate a b n = if n == 0 then [] else a :: alternate b a (n-1)

piece_to_i : Piece -> Int
piece_to_i piece =
  case piece of
  K -> 0
  Q -> 1
  R -> 2
  B -> 3
  N -> 4
  P -> 5

int_to_piece : Int -> Piece
int_to_piece piece =
  case piece of
  0 -> K
  1 -> Q
  2 -> R
  3 -> B
  4 -> N
  _ -> P

colorpiece_to_string : ColorPiece -> String
colorpiece_to_string cp =
    let piece_to_unicode base p = base + piece_to_i p |> Char.fromCode |> String.fromChar
        white_king = 9812
        black_king = white_king+6
    in case cp of
      (White, p) -> piece_to_unicode white_king p
      (Black, p) -> piece_to_unicode black_king p

piece_to_element: Model -> SquareIndex -> Element
piece_to_element { game, state, message } (f,r) =
  let colored_square = if (f+r) % 2 == 0 then black_square else white_square in
  case Chess.Chess.board_get game.board (f,r) of
    Nothing -> colored_square
    Just cp ->
      let piece_element = colorpiece_to_string cp |> unit_string
      in flow outward [colored_square, piece_element]

promotion_dropdown : Signal.Address Action -> Dict Int Move -> Color -> Element
promotion_dropdown address dict color =
  let menu_item (i, move) = (colorpiece_to_string (color, int_to_piece i), MoveSelected [move])
  in ("Cancel", ClearSelection) :: map menu_item (Dict.toList dict) |> dropDown (Signal.message address) |> container (8*unit) (8*unit) middle

groupBy : (v -> comparable) -> List v -> Dict comparable (List v)
groupBy getKey =
  let addEntry v dict = let k = getKey v in Dict.insert k (v :: withDefault [] (Dict.get k dict)) dict
  in List.foldl addEntry Dict.empty

indexBy : (v -> comparable) -> List v -> Dict comparable v
indexBy getKey = List.foldl (\v -> Dict.insert (getKey v) v) Dict.empty

add_handler: Signal.Address Action -> Model -> SquareIndex -> Element -> Element
add_handler address { game, state, message } square element =
  case state of
    PickPiece dict ->
      case Dict.get square dict of
        Nothing -> element
        Just moves -> PieceSelected moves |> Signal.message address |> flip clickable element
    PickDestination dict ->
      let response =
        case Maybe.withDefault [] (Dict.get square dict)  of
          [] -> ClearSelection
          xs -> MoveSelected xs
      in clickable (Signal.message address response) element
    PickPromotionPiece dict ->
      element

moves_to_element : Model -> Element
moves_to_element model =
  let moves_to_string n l =
        let format_string = fromString >> height (unit/4) >> leftAligned >> width unit
            numbered_move_to_string m = format_string (toString n ++ ". " ++ move_to_string m)
            move_pair w b = flow right [numbered_move_to_string w, format_string (move_to_string b)] in
        case l of
        w :: b :: l' -> move_pair w b :: moves_to_string (n+1) l'
        w :: [] -> [numbered_move_to_string w]
        [] -> []
  in List.reverse model.game.moves_played |>
     moves_to_string 1 |>
     flow down

model_to_element: Signal.Address Action -> Model -> Element
model_to_element address model =
  let file_to_element r f = piece_to_element model (f,r) |> add_handler address model (f,r)
      rank_legend = rank_index_to_string >> unit_string
      rank_to_element r = rank_legend r :: map (file_to_element r) [0..7]
      file_legend = unit_spacer :: map unit_string ["a", "b", "c", "d", "e", "f", "g", "h"]
      status_message = [unit_spacer, fromString model.message |> centered >> container (8*unit) unit middle]
      fen_message = [unit_spacer, game_to_FEN model.game |> fromString |> height (unit/5) |> centered |> container (8*unit) unit middle]
      board = status_message :: map rank_to_element [7,6,5,4,3,2,1,0] ++ [file_legend, fen_message] |> map (flow right) |> flow down
      moves = unit_spacer `above` (unit_spacer `beside` moves_to_element model)
      board' =
        case model.state of
        PickPromotionPiece dict -> flow outward [board, promotion_dropdown address dict model.game.to_move]
        _ -> board
  in board' `beside` moves

type Action = ClearSelection | PieceSelected (List Move)| MoveSelected (List Move) | OfferDraw | Resign

pawn_promotion: Game -> ColorPiece -> SquareIndex -> SquareIndex -> Bool
pawn_promotion game cp from (to_file, to_rank) =
  case (cp, to_rank) of
    ((Black, P), 0) -> True
    ((White, P), 7) -> True
    _ -> False

type State = PickPiece (Dict SquareIndex (List Move)) |
             PickDestination (Dict SquareIndex (List Move)) |
             PickPromotionPiece (Dict Int Move)

type alias Model = { game: Game, state: State, moves: List Move, message: String }

initial_model : Model
initial_model =
  let moves = legal_moves initial_game in { game = initial_game, moves = moves, state = PickPiece (groupBy .from moves), message = "" }

model : Model
model = initial_model

update : Action -> Model -> Model
update action model =
  case action of
    ClearSelection ->
      { model | state = PickPiece (groupBy .from model.moves), message = "selection cleared" }
    PieceSelected moves ->
      let message = map move_to_string moves |> String.join ", " in
      { model | state = PickDestination (groupBy .to moves), message = message }
    MoveSelected [] -> Debug.log  "Inconceivable" model
    MoveSelected (move :: []) ->
      let game' = make_move model.game move
          moves' = legal_moves game'
      in { game = game', state = PickPiece (groupBy .from moves'), moves = moves', message = "" }
    MoveSelected moves ->
      let getInfo move =
        case move.info of
        Just (Promotion p) -> piece_to_i p
        _ -> piece_to_i P
      in { model | state = PickPromotionPiece (indexBy getInfo moves), message = "pick what your pawn promotes to" }
    OfferDraw ->
      { model | state = PickPiece (groupBy .from model.moves), message = "offer declined" }
    Resign ->
      { initial_model | message = "better luck next time" }

view : Signal.Address Action -> Model -> Html
view address model = model_to_element address model |> fromElement

main = StartApp.Simple.start { model = initial_model, view = view, update = update }
