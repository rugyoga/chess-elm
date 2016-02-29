module Chess.Chess (Board, Color(..), ColorPiece, Game, Info(..), Move, Piece(..), SquareIndex, boardClear, boardGet, boardSet, gameToFEN, initialBoard, initialGame, legalMoves, makeMove, moveToString, opposite, rankIndexToString, squareToString) where
import Array exposing (toIndexedList)
import Dict exposing (Dict, fromList, get, insert, remove, union)
import List exposing (indexedMap, concatMap, filter, head, length, map, repeat, reverse, sortBy, take)
import Maybe exposing (Maybe, withDefault)
import String exposing (concat, join)

type alias Board element = Dict SquareIndex element
type Color = Black | White
type alias ColorPiece = (Color, Piece)
type Piece = P | N | B | R | Q | K
type alias FileIndex = Int
type alias RankIndex = Int
type alias SquareIndex = (FileIndex, RankIndex)
type alias FileDelta = Int
type alias RankDelta = Int
type alias Delta = (FileDelta, RankDelta)
type alias Move = { piece: Piece, from: SquareIndex, to: SquareIndex, info: Maybe Info, check: Bool, captured: Maybe Piece }
type Info = Promotion Piece | EnPassant | Castled
type alias Castling = { kingside: Bool, queenside: Bool }
type alias EachSide a = { white: a, black: a }
type alias Game = { board: Board ColorPiece,
                    attackedBy: Board (List (SquareIndex, ColorPiece)),
                    toMove: Color,
                    kings: EachSide SquareIndex,
                    castling: EachSide Castling,
                    enPassant: Maybe SquareIndex,
                    captureOrPawnMove: Int,
                    moveNumber: Int,
                    movesPlayed: List Move }

type alias MoveList = List (List Move)
type alias CandidateMoves = List (List SquareIndex)

getByColor : EachSide a -> Color -> a
getByColor { white, black } color =
  case color of
  White -> white
  Black -> black

initialBoard : Board ColorPiece
initialBoard =
  let mkRank rank color pieces = indexedMap (\file p -> ((file, rank), (color, p))) pieces |> fromList
      backRank = [R, N, B, Q, K, B, N, R]
      pawns = repeat 8 P in
  mkRank 0 White backRank `union` mkRank 1 White pawns `union` mkRank 7 Black backRank `union` mkRank 6 Black pawns

opposite : Color -> Color
opposite color =
  case color of
  White -> Black
  Black -> White

file : SquareIndex -> FileIndex
file = fst

rank : SquareIndex -> RankIndex
rank = snd

boardGet : Board a -> SquareIndex -> Maybe a
boardGet = flip get

boardSet : Board a -> SquareIndex -> a -> Board a
boardSet board square piece = insert square piece board

boardClear : Board a -> SquareIndex -> Board a
boardClear = flip remove

boardMove : Board a -> SquareIndex -> SquareIndex -> a -> Board a
boardMove board from to p = boardSet (boardClear board from) to p

isEmpty : Board a -> SquareIndex -> Bool
isEmpty board square = boardGet board square == Nothing

eachSquare : (SquareIndex -> a) -> Board a
eachSquare initSquare =
  concatMap (\file -> map (\rank -> let sq = (file, rank) in (sq, initSquare sq)) [0..7]) [0..7] |> Dict.fromList

onBoard : SquareIndex -> Bool
onBoard (file, rank) = 0 <= file && file < 8 && 0 <= rank && rank < 8

direction : SquareIndex -> Delta -> List SquareIndex
direction (f, r) ((fDelta, rDelta) as delta) =
  let candidate = (f + fDelta, r + rDelta) in
  if onBoard candidate then candidate :: direction candidate delta else []

pseudoMovesAt : Bool -> List Delta -> SquareIndex -> CandidateMoves
pseudoMovesAt onlyOne directions square =
  let squares = map (direction square) directions |> filter (List.isEmpty >> not) in
  if onlyOne then map (take 1) squares else sortBy length squares |> reverse

pseudoMoves : Bool -> List Delta -> Board CandidateMoves
pseudoMoves onlyOne directions = pseudoMovesAt onlyOne directions |> eachSquare

directionsB = [(1, 1), (1, -1), (-1, 1), (-1, -1)]
directionsR = [(0, 1), (0, -1), (1, 0), (-1, 0)]
directionsN = [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
directionsQ = directionsR ++ directionsB

movesB = pseudoMoves False directionsB
movesN = pseudoMoves True  directionsN
movesR = pseudoMoves False directionsR
movesQ = pseudoMoves False directionsQ
movesK = pseudoMoves True  directionsQ

moves : Piece -> Board CandidateMoves
moves piece =
  case piece of
  B -> movesB
  R -> movesR
  N -> movesN
  Q -> movesQ
  K -> movesK
  P -> Dict.empty

initialGame : Game
initialGame =
  let canCastle = { kingside = True, queenside = True } in
  { board             = initialBoard,
    attackedBy        = Dict.empty,
    toMove            = White,
    castling          = { white = canCastle, black = canCastle },
    kings             = { white = e1, black = e8 },
    enPassant         = Nothing,
    captureOrPawnMove = 0,
    moveNumber        = 1,
    movesPlayed       = [] }

toIndexedList : Board a -> List (SquareIndex, a)
toIndexedList = Dict.toList

isPawnCheck : Board ColorPiece -> Color -> SquareIndex -> Bool
isPawnCheck board color square = onBoard square && boardGet board square == Just (opposite color, K)

files = Array.fromList ["a","b","c","d","e","f","g","h"]

fileIndexToString : FileIndex -> String
fileIndexToString file = withDefault "" (Array.get file files)

rankIndexToString : RankIndex -> String
rankIndexToString rank = toString (rank+1)

squareToString : SquareIndex -> String
squareToString (file, rank) = fileIndexToString file ++ rankIndexToString rank

pieceToString : Piece -> String
pieceToString = toString

pawnInfoToString : Maybe Info -> String
pawnInfoToString info =
  case info of
  Just (Promotion piece) -> "=" ++ pieceToString piece
  Just EnPassant         -> " e.p."
  _                      -> ""

pawnMoveToString : SquareIndex -> SquareIndex -> String
pawnMoveToString from to =
  if file from /= file to
  then fileIndexToString (file from) ++ fileIndexToString (file to)
  else squareToString to

iff : Bool -> String -> String
iff p s = if p then s else ""

moveToString : Move -> String
moveToString { piece, from, to, info, check, captured } =
  let capturedToString = iff (captured /= Nothing) "x" in
  (case (piece, info) of
   (P, _)            -> pawnMoveToString from to ++ pawnInfoToString info
   (K, Just Castled) -> if file to < file from then "o-o-o" else "o-o"
   _                 -> pieceToString piece ++ capturedToString ++ squareToString to) ++
  iff check "+"

a = 0
b = 1
c = 2
d = 3
e = 4
f = 5
g = 6
h = 7
a1 = (a,0)
c1 = (c,0)
d1 = (d,0)
e1 = (e,0)
f1 = (f,0)
g1 = (g,0)
h1 = (h,0)
a8 = (a,7)
b8 = (b,7)
c8 = (c,7)
d8 = (d,7)
e8 = (e,7)
f8 = (f,7)
g8 = (g,7)
h8 = (h,7)

makeInfoMove : Maybe Info -> SquareIndex -> Color -> Board ColorPiece -> Board ColorPiece
makeInfoMove info to toMove board =
  case info of
  Just Castled ->
    let first = backRank toMove in
    if file to == c
    then boardMove board (a, first) (d, first) (toMove, R)
    else boardMove board (h, first) (f, first) (toMove, R)
  Just EnPassant -> boardClear board (file to, backward toMove (rank to))
  _ -> board

makeCastling : Piece -> SquareIndex -> Color -> EachSide Castling -> EachSide Castling
makeCastling  piece from toMove castling =
  let update original =
        if rank from /= backRank toMove then original else
        case (piece, file from) of
        (K, _) -> { queenside = False,                    kingside = False }
        (R, 0) -> { queenside = False,                    kingside = castling.white.kingside }
        (R, 7) -> { queenside = castling.white.queenside, kingside = False }
        _      -> original in
        case toMove of
        White -> { castling | white = update castling.white }
        Black -> { castling | black = update castling.black }

promotePiece : Piece -> Maybe Info -> Piece
promotePiece piece info =
  case info of
  Just (Promotion promotedPiece) -> promotedPiece
  _                              -> piece

computeEnPassant : Piece -> SquareIndex -> SquareIndex -> Color -> Maybe SquareIndex
computeEnPassant piece from to toMove =
  if piece == P && abs (rank from - rank to) == 2 then Just (file from, if toMove == White then 2 else 5) else Nothing

computeDrawCount piece captured count =
  if piece == P || captured /= Nothing then 0 else count + 1

makeMove : Game -> Move -> Game
makeMove ({ board, attackedBy, toMove, kings, castling, enPassant, captureOrPawnMove, moveNumber, movesPlayed} as game) ({ piece, from, to, info, check, captured } as move) =
  { board             = boardMove board from to (toMove, promotePiece piece info) |> makeInfoMove info to toMove,
    attackedBy        = Dict.empty,
    toMove            = opposite toMove,
    kings             = if piece /= K then kings else if toMove == White then { kings | white = to } else { kings | black = to },
    castling          = makeCastling piece from toMove castling,
    enPassant         = computeEnPassant piece from to toMove,
    captureOrPawnMove = computeDrawCount piece captured captureOrPawnMove,
    moveNumber        = if toMove == White then moveNumber + 1 else moveNumber,
    movesPlayed       = move :: movesPlayed }

pawnMove : Game -> Color -> SquareIndex -> SquareIndex -> Maybe Piece -> List Move
pawnMove game color from ((toFile, toRank) as to) captured =
  let check = isPawnCheck game.board color (qside toFile, forward color toRank) ||
              isPawnCheck game.board color (kside toFile, forward color toRank)
      move = { piece = P, from = from, to = to, info = Nothing, check = check, captured = captured }
      promotionsMoves = map (\p -> { move | info = Just (Promotion p)}) [Q,R,N,B] in
  case (color, toRank) of
  (White, 7) -> promotionsMoves
  (Black, 0) -> promotionsMoves
  _          -> [move]

pawnMovesForward : Game -> SquareIndex -> Color -> RankIndex -> List Move
pawnMovesForward game (file, rank) color second =
  let third = forward color rank in
  if isEmpty game.board (file, third)
  then pawnMove game color (file, rank) (file, third) Nothing ++
       let fourth = forward color third in
       if rank == second && isEmpty game.board (file, fourth)
       then pawnMove game color (file, rank) (file, fourth) Nothing
       else []
  else []

pawnMoveCapture : Game -> SquareIndex -> SquareIndex -> Color -> List Move
pawnMoveCapture game from to color =
  case boardGet game.board to of
  Just (capColor, capPiece) ->
    if capColor /= color
    then pawnMove game color from to (Just capPiece)
    else []
  Nothing -> []

pawnMovesCaptures : Game -> SquareIndex -> Color -> List Move
pawnMovesCaptures game (file, rank) color =
  pawnMoveCapture game (file, rank) (qside file, forward color rank) color ++
  pawnMoveCapture game (file, rank) (kside file, forward color rank) color

legalMovesForPawn : Game -> SquareIndex -> Color -> RankIndex -> List Move
legalMovesForPawn game square color second =
  pawnMovesForward game square color second ++
  pawnMovesCaptures game square color

enPassantMoves : Game -> List Move
enPassantMoves game =
  let enPassantMove from to = map (\m -> { m | info = Just EnPassant}) (pawnMove game game.toMove from to (Just P))
      checkEP color from to = if get from game.board == Just (color, P) then enPassantMove from to else []
      checkEPs color ((f,r) as to) = checkEP color (qside f, backward color r) to ++ checkEP color (kside f, backward color r) to in
  case game.enPassant of
  Nothing -> []
  Just to -> checkEPs game.toMove to

split : (a -> Bool) -> List a -> (List a, List a)
split p l =
  case l of
  []      -> ([], [])
  x :: xs -> if p x then let xs2 = split p xs in (x::fst xs2, snd xs2) else ([], l)

checkSearch : Board ColorPiece -> SquareIndex -> CandidateMoves -> Bool
checkSearch board square moves =
  let inCheck moveList =
        let (empty, nonEmpty) = split (isEmpty board) moveList in
        head nonEmpty == Just square in
  List.any inCheck moves

checkForCheck : Game -> Piece -> SquareIndex -> Bool
checkForCheck game piece square =
  let otherKing = opposite game.toMove |> getByColor game.kings in
  boardGet (moves piece) square |> withDefault [] |> checkSearch game.board otherKing

pieceMove game piece from captured to =
  { piece = piece, from = from, to = to, info = Nothing, check = checkForCheck game piece to, captured = captured }

legalMovesForPiece : Game -> SquareIndex -> ColorPiece -> List Move
legalMovesForPiece game square (color, piece) =
  let pseudoMoves = boardGet (moves piece) square |> withDefault []
      pseudoToReal movelist =
        let (empty, nonEmpty) = split (isEmpty game.board) movelist in
        map (pieceMove game piece square Nothing) empty ++
        (case head nonEmpty of
         Just squareX ->
           case boardGet game.board squareX of
           Just (colorX, pieceX) -> if color == opposite colorX then [pieceMove game piece square (Just pieceX) squareX] else []
           Nothing -> []
         Nothing -> []) in
  concatMap pseudoToReal pseudoMoves

legalMovesFor : Game -> (SquareIndex, ColorPiece) -> List Move
legalMovesFor game (square, cp) =
  case cp of
  (White, P)     -> legalMovesForPawn  game square White 1
  (Black, P)     -> legalMovesForPawn  game square Black 6
  (color, piece) -> legalMovesForPiece game square cp

inCheck : Game -> Bool
inCheck game =
  case game.movesPlayed of
  [] -> False
  move :: moves -> move.check

castlingMoves : Game -> List Move
castlingMoves game =
  let allEmpty = map (flip Dict.member game.board) >> List.any identity >> not
      castle from to = { piece = K, from = from, to = to, info = Just Castled, check = checkForCheck game R to, captured = Nothing }
      checkCastle can start mid finish = if can && allEmpty [mid, finish] then [castle start finish] else []
      checkCastles canCastle qb q k kb kn = checkCastle canCastle.queenside k q qb ++ checkCastle canCastle.kingside k kb kn in
  case game.toMove of
  White -> checkCastles game.castling.white c1 d1 e1 f1 g1
  Black -> checkCastles game.castling.black c8 d8 e8 f8 g8

qside : FileIndex -> FileIndex
qside f = f-1

kside : FileIndex -> FileIndex
kside f = f+1

forward : Color -> RankIndex -> RankIndex
forward color r =
  case color of
  White -> r+1
  Black -> r-1

backRank : Color -> RankIndex
backRank color =
  case color of
  White -> 0
  Black -> 7

backward : Color -> RankIndex -> RankIndex
backward color r = forward (opposite color) r

legalMoves : Game -> List Move
legalMoves game =
  (toIndexedList game.board |>
   filter (\(s, (c, p)) -> c == game.toMove) |>
   concatMap (legalMovesFor game)) ++
  (if inCheck game then [] else castlingMoves game) ++
  enPassantMoves game

castlingToString : EachSide Castling -> String
castlingToString c =
  let toString side = iff side.kingside "K" ++ iff side.queenside "Q" in
  toString c.white ++ toLower (toString c.black)

enPassantToString : Maybe SquareIndex -> String
enPassantToString ep =
  case ep of
  Nothing -> "-"
  Just (square) -> squareToString square

toMoveToString : Color -> String
toMoveToString toMove =
  case toMove of
  White -> "w"
  Black -> "b"

gameToFEN: Game -> String
gameToFEN { board, attackedBy, toMove, kings, castling, enPassant, captureOrPawnMove, moveNumber } =
  let rankToList rank = map (\file -> Dict.get (file,rank) board) [0..7]
      colorpieceToString (c,p) =
        let s = pieceToString p in
        if c == White then s else String.toLower s
      rankToString n l =
        case l of
        []           -> if n > 0 then toString n else ""
        Nothing::cps -> rankToString (n+1) cps
        Just cp::cps -> rankToString n [] ++ colorpieceToString cp ++ rankToString 0 cps
      boardToFEN board = map (rankToList >> rankToString 0) [0..7] |> reverse |> join "/" in
  join " " [boardToFEN board, toMoveToString toMove, castlingToString castling, enPassantToString enPassant, toString captureOrPawnMove, toString moveNumber]
