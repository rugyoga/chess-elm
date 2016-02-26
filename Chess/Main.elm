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

import Chess.Chess exposing (Board, Color(..), ColorPiece, Game, Info(..), Move, Piece(..), SquareIndex, boardClear, boardGet, boardSet, gameToFEN, initialGame, legalMoves, makeMove, moveToString, opposite, rankIndexToString, squareToString)

square n = size n n empty
unit = 60
unitSquare = square unit
unitSpacer = spacer unit unit
unitString = fromString >> typeface ["arial", "tahoma", "helvetica"] >> height (0.75 * unit) >> centered >> container unit unit middle
whiteSquare = color Color.lightGrey unitSquare
blackSquare = color Color.darkGrey unitSquare
legend s = unitSquare

pieceToInt : Piece -> Int
pieceToInt piece =
  case piece of
  K -> 0
  Q -> 1
  R -> 2
  B -> 3
  N -> 4
  P -> 5

intToPiece : Int -> Piece
intToPiece piece =
  case piece of
  0 -> K
  1 -> Q
  2 -> R
  3 -> B
  4 -> N
  _ -> P

colorpieceToString : ColorPiece -> String
colorpieceToString cp =
    let pieceToUnicode base p = base + pieceToInt p |> Char.fromCode |> String.fromChar
        whiteKing = 9812
        blackKing = whiteKing+6
    in case cp of
      (White, p) -> pieceToUnicode whiteKing p
      (Black, p) -> pieceToUnicode blackKing p

pieceToElement: Model -> SquareIndex -> Element
pieceToElement { game, state, message } (f,r) =
  let coloredSquare = if (f+r) % 2 == 0 then blackSquare else whiteSquare in
  case Chess.Chess.boardGet game.board (f,r) of
    Nothing -> coloredSquare
    Just cp ->
      let pieceElement = colorpieceToString cp |> unitString
      in flow outward [coloredSquare, pieceElement]

promotionDropdown : Signal.Address Action -> Dict Int Move -> Color -> Element
promotionDropdown address dict color =
  let menuItem (i, move) = (colorpieceToString (color, intToPiece i), MoveSelected [move])
  in ("Cancel", ClearSelection) :: map menuItem (Dict.toList dict) |> dropDown (Signal.message address) |> container (8*unit) (8*unit) middle

groupBy : (v -> comparable) -> List v -> Dict comparable (List v)
groupBy getKey =
  let addEntry v dict = let k = getKey v in Dict.insert k (v :: withDefault [] (Dict.get k dict)) dict
  in List.foldl addEntry Dict.empty

indexBy : (v -> comparable) -> List v -> Dict comparable v
indexBy getKey = List.foldl (\v -> Dict.insert (getKey v) v) Dict.empty

addHandler: Signal.Address Action -> Model -> SquareIndex -> Element -> Element
addHandler address { game, state, message } square element =
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

movesToElement : Model -> Element
movesToElement model =
  let movesToString n l =
        let formatString = fromString >> height (unit/4) >> leftAligned >> width unit
            numberedMoveToString m = toString n ++ ". " ++ moveToString m |> formatString
            movePair w b = flow right [numberedMoveToString w, moveToString b |> formatString] in
        case l of
        w :: b :: l' -> movePair w b :: movesToString (n+1) l'
        w :: []      -> [numberedMoveToString w]
        []           -> []
  in List.reverse model.game.movesPlayed |> movesToString 1 |> flow down

modelToElement: Signal.Address Action -> Model -> Element
modelToElement address model =
  let fileToElement r f = pieceToElement model (f,r) |> addHandler address model (f,r)
      rankLegend = rankIndexToString >> unitString
      rankToElement r = rankLegend r :: map (fileToElement r) [0..7]
      fileLegend = unitSpacer :: map unitString ["a", "b", "c", "d", "e", "f", "g", "h"]
      statusMessage = [unitSpacer, fromString model.message |> centered >> container (8*unit) unit middle]
      fenMessage = [unitSpacer, gameToFEN model.game |> fromString |> height (unit/5) |> centered |> container (8*unit) unit middle]
      board = statusMessage :: map rankToElement [7,6,5,4,3,2,1,0] ++ [fileLegend, fenMessage] |> map (flow right) |> flow down
      moves = unitSpacer `above` (unitSpacer `beside` movesToElement model)
      board' =
        case model.state of
        PickPromotionPiece dict -> flow outward [board, promotionDropdown address dict model.game.toMove]
        _ -> board
  in board' `beside` moves

type Action = ClearSelection | PieceSelected (List Move)| MoveSelected (List Move) | OfferDraw | Resign

pawnPromotion: Game -> ColorPiece -> SquareIndex -> SquareIndex -> Bool
pawnPromotion game cp from (toFile, toRank) =
  case (cp, toRank) of
    ((Black, P), 0) -> True
    ((White, P), 7) -> True
    _ -> False

type State = PickPiece (Dict SquareIndex (List Move)) |
             PickDestination (Dict SquareIndex (List Move)) |
             PickPromotionPiece (Dict Int Move)

type alias Model = { game: Game, state: State, moves: List Move, message: String }

initialModel : Model
initialModel =
  let moves = legalMoves initialGame in { game = initialGame, moves = moves, state = PickPiece (groupBy .from moves), message = "" }

model : Model
model = initialModel

update : Action -> Model -> Model
update action model =
  case action of
    ClearSelection ->
      { model | state = PickPiece (groupBy .from model.moves), message = "selection cleared" }
    PieceSelected moves ->
      let message = map moveToString moves |> String.join ", " in
      { model | state = PickDestination (groupBy .to moves), message = message }
    MoveSelected [] -> Debug.log  "Inconceivable" model
    MoveSelected (move :: []) ->
      let game' = makeMove model.game move
          moves' = legalMoves game'
      in { game = game', state = PickPiece (groupBy .from moves'), moves = moves', message = "" }
    MoveSelected moves ->
      let getInfo move =
        case move.info of
        Just (Promotion p) -> pieceToInt p
        _ -> pieceToInt P
      in { model | state = PickPromotionPiece (indexBy getInfo moves), message = "pick what your pawn promotes to" }
    OfferDraw ->
      { model | state = PickPiece (groupBy .from model.moves), message = "offer declined" }
    Resign ->
      { initialModel | message = "better luck next time" }

view : Signal.Address Action -> Model -> Html
view address model = modelToElement address model |> fromElement

main = StartApp.Simple.start { model = initialModel, view = view, update = update }
