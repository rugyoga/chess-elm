module Chess.Chess (Board, Color(..), ColorPiece, Game, Info(..), Move, Piece(..), SquareIndex, boardClear, boardGet, boardSet, gameToFEN, initialBoard, initialGame, legalMoves, makeMove, moveToString, opposite, rankIndexToString, squareToString) where
import Array exposing (toIndexedList)
import Dict exposing (Dict, fromList, get, insert, remove, union)
import List exposing (indexedMap, map, repeat, take)
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
type alias CanCastle = { kingside: Bool, queenside: Bool }
type alias Castling = { white: CanCastle, black: CanCastle }
type alias Game = { board: Board ColorPiece,
                    attacked_by: Board (List (SquareIndex, ColorPiece)),
                    to_move: Color,
                    castling: Castling,
                    en_passant: Maybe SquareIndex,
                    capture_or_pawn_move: Int,
                    move_number: Int,
                    moves_played: List Move }

type alias MoveList = List (List Move)
type alias CandidateMoves = List (List SquareIndex)

initialBoard : Board ColorPiece
initialBoard =
  let mkRank rank color pieces = indexedMap (\file p -> ((file, rank), (color, p))) pieces |> fromList
      backRank = [R, N, B, Q, K, B, N, R]
      pawns = repeat 8 P
  in mkRank 0 White backRank `union` mkRank 1 White pawns `union` mkRank 7 Black backRank `union` mkRank 6 Black pawns

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

isEmpty : Board a -> SquareIndex -> Bool
isEmpty board square = boardGet board square == Nothing

eachSquare : (SquareIndex -> a) -> Board a
eachSquare init_square =
  map (\file -> map (\rank -> let sq = (file, rank) in (sq, init_square sq)) [0..7]) [0..7] |> List.concat |> Dict.fromList

onBoard : SquareIndex -> Bool
onBoard (file, rank) = 0 <= file && file < 8 && 0 <= rank && rank < 8

direction : SquareIndex -> Delta -> List SquareIndex
direction ((f, r) as square) ((f_d, r_d) as delta) =
  let candidate = (f + f_d, r + r_d) in
  if onBoard candidate then candidate :: direction candidate delta else []

pseudoMoves : Bool -> List Delta -> Board CandidateMoves
pseudoMoves only_one directions =
  let gen_square square =
        let squares = map (direction square) directions |> List.filter (List.isEmpty >> not) in
        if only_one then map (take 1) squares else List.sortBy List.length squares |> List.reverse in
  eachSquare gen_square

directionsB = [(1, 1), (1, -1), (-1, 1), (-1, -1)]
directionsR = [(0, 1), (0, -1), (1, 0), (-1, 0)]
directionsN = [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
directionsQ = directionsR ++ directionsB

moves : Piece -> Board CandidateMoves
moves piece =
  case piece of
  B -> pseudoMoves False directionsB
  R -> pseudoMoves False directionsR
  N -> pseudoMoves True  directionsN
  Q -> pseudoMoves False directionsQ
  K -> pseudoMoves True  directionsQ
  P -> Dict.empty

initialGame : Game
initialGame =
  let can_castle = { kingside = True, queenside = True }
  in { board = initialBoard,
       attacked_by = Dict.empty,
       to_move = White,
       castling = { white = can_castle, black = can_castle },
       en_passant = Nothing,
       capture_or_pawn_move = 0,
       move_number = 1,
       moves_played = [] }

toIndexedList : Board a -> List (SquareIndex, a)
toIndexedList = Dict.toList

isCheck : Board ColorPiece -> Color -> SquareIndex -> Bool
isCheck board color square = onBoard square && boardGet board square == Just (opposite color, K)

files = Array.fromList ["a","b","c","d","e","f","g","h"]

fileIndexToString : FileIndex -> String
fileIndexToString file = withDefault "" (Array.get file files)

rankIndexToString : RankIndex -> String
rankIndexToString rank = toString (rank+1)

squareToString : SquareIndex -> String
squareToString (file, rank) = fileIndexToString file ++ rankIndexToString rank

pieceToString : Piece -> String
pieceToString = toString

moveToString : Move -> String
moveToString { piece, from, to, info, check, captured } =
  case piece of
  P ->
    (if file from /= file to
     then fileIndexToString (file from) ++ fileIndexToString (file to) ++ "x" ++ pieceToString (withDefault P captured)
     else squareToString to) ++
    (case info of
       Just (Promotion piece) -> "=" ++ pieceToString piece
       Just EnPassant -> "e.p."
       _ -> "") ++
    (if check then "+" else "")
  _ ->
    if piece == K && info == Just Castled
    then if file to < file from then "o-o-o" else "o-o"
    else
    let captured_to_string =
          case captured of
            Nothing -> ""
            Just piece -> "x" ++ pieceToString piece
        check_to_string = if check then "+" else ""
    in pieceToString piece ++ squareToString to ++ captured_to_string ++ check_to_string

a1 = (0,0)
c1 = (2,0)
d1 = (3,0)
e1 = (4,0)
f1 = (5,0)
g1 = (6,0)
h1 = (7,0)
a8 = (0,7)
b8 = (1,7)
c8 = (2,7)
d8 = (3,7)
e8 = (4,7)
f8 = (5,7)
g8 = (6,7)
h8 = (7,7)

makeMove : Game -> Move -> Game
makeMove ({ board, attacked_by, to_move, castling, en_passant, capture_or_pawn_move, move_number, moves_played} as game) ({ piece, from, to, info, check, captured } as move) =
  let piece' =
        case info of
          Just (Promotion promoted_piece) -> promoted_piece
          _ -> piece
      board' = boardSet (boardClear board from) to (to_move, piece')
      board'' =
        case (to_move, info) of
        (White, Just Castled) ->
          if to == c1
          then boardSet (boardClear board' a1) d1 (White, R)
          else boardSet (boardClear board' h1) f1 (White, R)
        (Black, Just Castled) ->
          if to == c8
          then boardSet (boardClear board' a8) d8 (Black, R)
          else boardSet (boardClear board' h8) f8 (Black, R)
        (White, Just EnPassant) ->
          boardClear board' (file to, rank to - 1)
        (Black, Just EnPassant) ->
          boardClear board' (file to, rank to + 1)
        _ -> board'
      to_move' = opposite to_move
      castling' =
        case (to_move, piece, from) of
          (White, K, _)     -> { white = { queenside = False, kingside = False }, black = castling.black }
          (White, R, (0,0)) -> { white = { queenside = False, kingside = castling.white.kingside }, black = castling.black }
          (White, R, (7,0)) -> { white = { queenside = castling.white.queenside, kingside = False }, black = castling.black }
          (Black, K, _)     -> { white = castling.white, black = { queenside = False, kingside = False } }
          (Black, R, (0,7)) -> { white = castling.white, black = { queenside = False, kingside = castling.black.kingside } }
          (Black, R, (7,7)) -> { white = castling.white, black = { queenside = castling.black.queenside, kingside = False } }
          _                 -> castling
      en_passant' = if piece == P && abs (rank from - rank to) == 2 then Just (file from, if to_move == White then 2 else 5) else Nothing
      capture_or_pawn_move' = if piece == P || captured /= Nothing then 0 else capture_or_pawn_move + 1
      move_number' = if to_move == White then move_number + 1 else move_number
      moves_played' = move :: moves_played
  in { board = board'', attacked_by = Dict.empty, to_move = to_move', castling = castling',
       en_passant = en_passant', capture_or_pawn_move = capture_or_pawn_move',
       move_number = move_number', moves_played = moves_played'}


pawnMove : Board ColorPiece -> Color -> SquareIndex -> SquareIndex -> FileDelta -> Maybe Piece -> List Move
pawnMove board color from ((to_f, to_r) as to) dir captured =
  let check = isCheck board color (qside to_f, to_r+dir) ||
              isCheck board color (kside to_f, to_r+dir)
      move = { piece = P, from = from, to = to, info = Nothing, check = check, captured = captured }
      promotions_moves = map (\p -> { move | info = Just (Promotion p)}) [Q,R,N,B]
  in case (color, to_r) of
       (White, 7) -> promotions_moves
       (Black, 0) -> promotions_moves
       _ -> [move]

pawnMovesForward : Board ColorPiece -> SquareIndex -> Color -> RankIndex -> RankDelta -> List Move
pawnMovesForward board (file, rank) color second dir =
  let third = rank+dir in
  if isEmpty board (file, third)
  then pawnMove board color (file, rank) (file, third) dir Nothing ++
       let fourth = third+dir in
       if rank == second && isEmpty board (file, fourth)
       then pawnMove board color (file, rank) (file, fourth) dir Nothing
       else []
  else []

pawnMoveCapture : Board ColorPiece -> SquareIndex -> SquareIndex -> RankDelta -> Color -> List Move
pawnMoveCapture board from to dir color =
  case boardGet board to of
    Just (cap_color, cap_piece) ->
      if cap_color /= color
      then pawnMove board color from to dir (Just cap_piece)
      else []
    Nothing -> []

pawnMovesCaptures : Board ColorPiece -> SquareIndex -> Color -> RankIndex -> List Move
pawnMovesCaptures board (file, rank) color dir =
  pawnMoveCapture board (file, rank) (qside file, rank+dir) dir color ++
  pawnMoveCapture board (file, rank) (kside file, rank+dir) dir color

legalMovesForPawn : Board ColorPiece -> SquareIndex -> Color -> RankIndex -> RankDelta -> List Move
legalMovesForPawn board square color second dir =
  pawnMovesForward board square color second dir ++
  pawnMovesCaptures board square color dir

split : (a -> Bool) -> List a -> (List a, List a)
split p l =
  case l of
    [] -> ([], [])
    x :: xs -> if p x then let xs2 = split p xs in (x::fst xs2, snd xs2) else ([], l)

pieceMove piece from captured to =
  { piece = piece, from = from, to = to, info = Nothing, check = False, captured = captured }

legalMovesForPiece : Board ColorPiece -> SquareIndex -> ColorPiece -> List Move
legalMovesForPiece board square (color, piece) =
  let pseudoMoves = withDefault [] (boardGet (moves piece) square)
      pseudoToReal movelist =
        let (empty, nonEmpty) = split (isEmpty board) movelist
        in map (pieceMove piece square Nothing) empty ++
           (case List.head nonEmpty of
              Just square_x ->
                case boardGet board square_x of
                  Just (color_x, piece_x) -> if color == opposite color_x then [pieceMove piece square (Just piece_x) square_x] else []
                  Nothing -> []
              Nothing -> [])
  in List.concat (map pseudoToReal pseudoMoves)

legalMovesFor : Board ColorPiece -> (SquareIndex, ColorPiece) -> List Move
legalMovesFor board (square, cp) =
  case cp of
    (White, P)     -> legalMovesForPawn board square White 1 1
    (Black, P)     -> legalMovesForPawn board square Black 6 -1
    (color, piece) -> legalMovesForPiece board square cp

castlingMoves : Game -> List Move
castlingMoves game =
  let are_empty = map (flip Dict.member game.board) >> List.any identity >> not
      castle from to = { piece = K, from = from, to = to, info = Just Castled, check = False, captured = Nothing }
      check_castle can start mid finish = if can && are_empty [mid, finish] then [castle start finish] else []
      check_castles can_castle qb q k kb kn = check_castle can_castle.queenside k q qb ++ check_castle can_castle.kingside k kb kn
  in case (game.to_move) of
     White -> check_castles game.castling.white c1 d1 e1 f1 g1
     Black -> check_castles game.castling.black c8 d8 e8 f8 g8

qside : FileIndex -> FileIndex
qside f = f-1

kside : FileIndex -> FileIndex
kside f = f-1

forward : Color -> RankIndex -> RankIndex
forward color r =
  case color of
  White -> r+1
  Black -> r-1

backward : Color -> RankIndex -> RankIndex
backward color r = forward (opposite color) r

enPassantMoves : Game -> List Move
enPassantMoves game =
  let en_passant_move from to = { piece = P, from = from, to = to, info = Just EnPassant, check = False, captured = Just P }
      check_ep color from to = if get from game.board == Just (White, P) then [en_passant_move from to] else []
      check_eps color ((f,r) as to) = check_ep color (qside f, backward color r) to ++ check_ep color (kside f, backward color r) to
  in case game.en_passant of
     Nothing -> []
     Just to -> check_eps game.to_move to

legalMoves : Game -> List Move
legalMoves game =
  (toIndexedList game.board |>
   List.filter (\(s, (c, p)) -> c == game.to_move) |>
   List.map (legalMovesFor game.board) |>
   List.concat) ++
  castlingMoves game ++
  enPassantMoves game

castlingToString : Castling -> String
castlingToString c =
  let s p str = if p then str else ""
  in s c.white.kingside "K" ++ s c.white.queenside "Q" ++ s c.black.kingside "k" ++ s c.black.queenside "q"

enPassantToString : Maybe SquareIndex -> String
enPassantToString ep =
  case ep of
    Nothing -> "-"
    Just (square) -> squareToString square

toMoveToString : Color -> String
toMoveToString to_move =
  case to_move of
    White -> "w"
    Black -> "b"

gameToFEN: Game -> String
gameToFEN { board, attacked_by, to_move, castling, en_passant, capture_or_pawn_move, move_number } =
  let rank_to_list rank = List.map (\file -> Dict.get (file,rank) board) [0..7]
      colorpiece_to_string (c,p) =
        let s = pieceToString p in
        if c == White then s else String.toLower s
      rank_to_string n l =
        case l of
          [] -> if n > 0 then toString n else ""
          Nothing::cps -> rank_to_string (n+1) cps
          Just cp::cps -> rank_to_string n [] ++ colorpiece_to_string cp ++ rank_to_string 0 cps
      board_to_FEN board =
        List.map rank_to_list [0..7] |>
        List.map (rank_to_string 0) |>
        List.reverse |>
        String.join "/"
  in String.join " " [board_to_FEN board, toMoveToString to_move, castlingToString castling, enPassantToString en_passant, toString capture_or_pawn_move, toString move_number]
