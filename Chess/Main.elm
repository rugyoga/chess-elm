import Array exposing (get)
import Char
import Color
import Dict exposing (Dict, insert)
import Element exposing (Element, above, below, beside, centered, color,
    container, down, empty, flow, leftAligned, middle,
    opacity, outward, right, size, spacer, toHtml, width)
-- import Graphics.Input exposing ()
import Html exposing (Html, button)
import Html.Events exposing (onClick)
import List exposing (filter, map, map2, member, range)
import Maybe exposing (withDefault)
-- import StartApp.Simple exposing (start)
import String exposing (toLower)
import Text exposing (fromString, height, monospace, typeface)

import Chess.Chess exposing (..)

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
pieceToElement model (f,r) =
  let coloredSquare = if (f+r) % 2 == 0 then blackSquare else whiteSquare in
  case Chess.Chess.boardGet (getGameWithMoves model).game.board (f,r) of
    Nothing -> coloredSquare
    Just cp ->
      let pieceElement = colorpieceToString cp |> unitString
      in flow outward [coloredSquare, pieceElement]

-- promotionDropdown : Signal.Address Action -> Dict Int Move -> Color -> Element
-- promotionDropdown address dict color =
--   let menuItem (i, move) = (colorpieceToString (color, intToPiece i), MoveSelected [move])
--   in ("Cancel", ClearSelection) :: map menuItem (Dict.toList dict) |> dropDown (Signal.message address) |> container (8*unit) (8*unit) middle

groupBy : (v -> comparable) -> List v -> Dict comparable (List v)
groupBy getKey =
  let addEntry v dict = let k = getKey v in Dict.insert k (v :: withDefault [] (Dict.get k dict)) dict
  in List.foldl addEntry Dict.empty

indexBy : (v -> comparable) -> List v -> Dict comparable v
indexBy getKey = List.foldl (\v -> Dict.insert (getKey v) v) Dict.empty

addHandler: Model -> SquareIndex -> Element -> Element
addHandler model square element =
  case model.state of
    PickPiece dict ->
      case Dict.get square dict of
        Nothing    -> element
        Just moves -> PieceSelected moves |> flip clickable element
    PickDestination dict ->
      let response =
        case Maybe.withDefault [] (Dict.get square dict)  of
          [] -> ClearSelection
          xs -> MoveSelected xs
      in clickable element
    PickPromotionPiece dict ->
      element

movesToElement : Model -> Element
movesToElement model =
  let movesToString n l =
        let formatString = fromString >> height (unit/4) >> leftAligned >> width unit
            nMoveToString m = toString n ++ ". " ++ moveToString m |> formatString
            movePair w b = flow right [nMoveToString w, moveToString b |> formatString] in
        case l of
        w :: b :: l2 -> movePair w b :: movesToString (n+1) l2
        w :: []      -> [nMoveToString w]
        []           -> []
  in List.reverse (getGameWithMoves model).game.movesPlayed |> movesToString 1 |> flow down

modelToElement: Model -> Element
modelToElement model =
  let fileToElement r f = pieceToElement model (f,r) |> addHandler model (f,r)
      rankLegend = rankIndexToString >> unitString
      rankToElement r = rankLegend r :: map (fileToElement r) (range 0 7)
      fileLegend = unitSpacer :: map unitString ["a", "b", "c", "d", "e", "f", "g", "h"]
      statusMessage = [unitSpacer, fromString model.message |> centered >> container (8*unit) unit middle]
      fenMessage = [unitSpacer, getGameWithMoves model |> .game |> gameToFEN |> fromString |> height (unit/5) |> centered |> container (8*unit) unit middle]
      board = statusMessage :: map rankToElement [7,6,5,4,3,2,1,0] ++ [fileLegend, fenMessage] |> map (flow right) |> flow down
      moves = above unitSpacer (beside unitSpacer (movesToElement model))
      board2 =
        case model.state of
        PickPromotionPiece dict -> flow outward [board, promotionDropdown dict (getGameWithMoves model).game.toMove]
        _                       -> board
  in beside board2 moves

type Msg = ClearSelection | PieceSelected (List Move)| MoveSelected (List Move) | OfferDraw | Resign

pawnPromotion: Game -> ColorPiece -> SquareIndex -> SquareIndex -> Bool
pawnPromotion game cp from (toFile, toRank) =
  case (cp, toRank) of
    ((Black, P), 0) -> True
    ((White, P), 7) -> True
    _ -> False

type State = PickPiece (Dict SquareIndex (List Move)) |
             PickDestination (Dict SquareIndex (List Move)) |
             PickPromotionPiece (Dict Int Move)

type alias GameWithMoves = { game : Game, moves: List Move }

type alias Model = { history: List GameWithMoves, state: State, message: String }

initialGameWithMoves : GameWithMoves
initialGameWithMoves = let moves = legalMoves initialGame in {game = initialGame, moves = moves}

initialModel : Model
initialModel = { history = [initialGameWithMoves], state = PickPiece (groupBy .from initialGameWithMoves.moves), message = "" }

model : Model
model = initialModel

getGameWithMoves : Model -> GameWithMoves
getGameWithMoves model = List.head model.history |> withDefault initialGameWithMoves

update : Msg -> Model -> Model
update msg model =
  let previous = getGameWithMoves model in
  case msg of
    ClearSelection ->
      { model | state = PickPiece (groupBy .from previous.moves), message = "selection cleared" }
    PieceSelected moves ->
      let message = map moveToString moves |> String.join ", " in
      { model | state = PickDestination (groupBy .to moves), message = message }
    MoveSelected [] -> Debug.log  "Inconceivable" model
    MoveSelected (move :: []) ->
      let game2  = makeMove previous.game move
          moves2 = legalMoves game2 in
      { history = { game = game2, moves = moves2} :: model.history, state = PickPiece (groupBy .from moves2), message = "" }
    MoveSelected moves ->
      let getInfo move =
        case move.info of
        Just (Promotion p) -> pieceToInt p
        _                  -> pieceToInt P in
      { model | state = PickPromotionPiece (indexBy getInfo moves), message = "pick what your pawn promotes to" }
    OfferDraw ->
      { model | state = PickPiece (groupBy .from previous.moves), message = "offer declined" }
    Resign ->
      { initialModel | message = "better luck next time" }

view : Model -> Html Msg
view  model = modelToElement model |> Element.toHtml

main = Html.beginnerProgram { model = initialModel, view = view, update = update }
