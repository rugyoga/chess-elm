module Chess.Chess (Board, Color(..), ColorPiece, Game, Info(..), Move, Piece(..), SquareIndex, board_clear, board_get, board_set, game_to_FEN, initial_board, initial_game, legal_moves, make_move, move_to_string, opposite, rank_index_to_string, square_to_string) where
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
type alias Game = { board: Board ColorPiece, to_move: Color, castling: Castling,
 en_passant: Maybe SquareIndex, capture_or_pawn_move: Int, move_number: Int, moves_played: List Move }

type alias MoveList = List (List Move)
type alias CandidateMoves = List (List SquareIndex)

initial_board : Board ColorPiece
initial_board =
  let mk_rank rank color pieces = indexedMap (\file p -> ((file, rank), (color, p))) pieces |> fromList
      back_rank = [R, N, B, Q, K, B, N, R]
      pawns = repeat 8 P
  in mk_rank 0 White back_rank `union` mk_rank 1 White pawns `union` mk_rank 7 Black back_rank `union` mk_rank 6 Black pawns

opposite : Color -> Color
opposite color =
  case color of
    White -> Black
    Black -> White

file : SquareIndex -> FileIndex
file = fst

rank : SquareIndex -> RankIndex
rank = snd

board_get : Board a -> SquareIndex -> Maybe a
board_get = flip get

board_set : Board a -> SquareIndex -> a -> Board a
board_set board square piece = insert square piece board

board_clear : Board a -> SquareIndex -> Board a
board_clear = flip remove

isEmpty : Board a -> SquareIndex -> Bool
isEmpty board square = board_get board square == Nothing

each_square : (SquareIndex -> a) -> Board a
each_square init_square =
  map (\file -> map (\rank -> let sq = (file, rank) in (sq, init_square sq)) [0..7]) [0..7] |> List.concat |> Dict.fromList

on_board : SquareIndex -> Bool
on_board (file, rank) = 0 <= file && file < 8 && 0 <= rank && rank < 8

direction : SquareIndex -> Delta -> List SquareIndex
direction ((f, r) as square) ((f_d, r_d) as delta) =
  let candidate = (f + f_d, r + r_d) in
  if on_board candidate then candidate :: direction candidate delta else []

pseudo_moves : Bool -> List Delta -> Board CandidateMoves
pseudo_moves only_one directions =
  let gen_square square =
        let squares = map (direction square) directions |> List.filter (List.isEmpty >> not) in
        if only_one then map (take 1) squares else List.sortBy List.length squares |> List.reverse in
  each_square gen_square

directions_b = [(1, 1), (1, -1), (-1, 1), (-1, -1)]
directions_r = [(0, 1), (0, -1), (1, 0), (-1, 0)]
directions_n = [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
directions_q = directions_r ++ directions_b

moves : Piece -> Board CandidateMoves
moves piece =
  case piece of
  B -> pseudo_moves False directions_b
  R -> pseudo_moves False directions_r
  N -> pseudo_moves True  directions_n
  Q -> pseudo_moves False directions_q
  K -> pseudo_moves True  directions_q
  P -> Dict.empty

initial_game : Game
initial_game =
  let can_castle = { kingside = True, queenside = True }
  in { board = initial_board,
       to_move = White,
       castling = { white = can_castle, black = can_castle },
       en_passant = Nothing,
       capture_or_pawn_move = 0,
       move_number = 1,
       moves_played = [] }

toIndexedList : Board a -> List (SquareIndex, a)
toIndexedList = Dict.toList

is_check : Board ColorPiece -> Color -> SquareIndex -> Bool
is_check board color square = on_board square && board_get board square == Just (opposite color, K)

files = Array.fromList ["a","b","c","d","e","f","g","h"]

file_index_to_string : FileIndex -> String
file_index_to_string file = withDefault "" (Array.get file files)

rank_index_to_string : RankIndex -> String
rank_index_to_string rank = toString (rank+1)

square_to_string : SquareIndex -> String
square_to_string (file, rank) = file_index_to_string file ++ rank_index_to_string rank

piece_to_string : Piece -> String
piece_to_string = toString

move_to_string : Move -> String
move_to_string { piece, from, to, info, check, captured } =
  case piece of
  P ->
    (if file from /= file to
     then file_index_to_string (file from) ++ file_index_to_string (file to) ++ "x" ++ piece_to_string (withDefault P captured)
     else square_to_string to) ++
    (case info of
       Just (Promotion piece) -> "=" ++ piece_to_string piece
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
            Just piece -> "x" ++ piece_to_string piece
        check_to_string = if check then "+" else ""
    in piece_to_string piece ++ square_to_string to ++ captured_to_string ++ check_to_string

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

make_move : Game -> Move -> Game
make_move ({ board, to_move, castling, en_passant, capture_or_pawn_move, move_number, moves_played} as game) ({ piece, from, to, info, check, captured } as move) =
  let piece' =
        case info of
          Just (Promotion promoted_piece) -> promoted_piece
          _ -> piece
      board' = board_set (board_clear board from) to (to_move, piece')
      board'' =
        case (to_move, info) of
        (White, Just Castled) ->
          if to == c1
          then board_set (board_clear board' a1) d1 (White, R)
          else board_set (board_clear board' h1) f1 (White, R)
        (Black, Just Castled) ->
          if to == c8
          then board_set (board_clear board' a8) d8 (Black, R)
          else board_set (board_clear board' h8) f8 (Black, R)
        (White, Just EnPassant) ->
          board_clear board' (file to, rank to - 1)
        (Black, Just EnPassant) ->
          board_clear board' (file to, rank to + 1)
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
  in { board = board'', to_move = to_move', castling = castling',
       en_passant = en_passant', capture_or_pawn_move = capture_or_pawn_move',
       move_number = move_number', moves_played = moves_played'}


pawn_move : Board ColorPiece -> Color -> SquareIndex -> SquareIndex -> FileDelta -> Maybe Piece -> List Move
pawn_move board color from ((to_f, to_r) as to) dir captured =
  let check = is_check board color (to_f-1, to_r+dir) ||
              is_check board color (to_f+1, to_r+dir)
      move = { piece = P, from = from, to = to, info = Nothing, check = check, captured = captured }
      promotions_moves = map (\p -> { move | info = Just (Promotion p)}) [Q,R,N,B]
  in case (color, to_r) of
       (White, 7) -> promotions_moves
       (Black, 0) -> promotions_moves
       _ -> [move]

pawn_moves_forward : Board ColorPiece -> SquareIndex -> Color -> RankIndex -> RankDelta -> List Move
pawn_moves_forward board (file, rank) color second dir =
  let third = rank+dir in
  if isEmpty board (file, third)
  then pawn_move board color (file, rank) (file, third) dir Nothing ++
       let fourth = third+dir in
       if rank == second && isEmpty board (file, fourth)
       then pawn_move board color (file, rank) (file, fourth) dir Nothing
       else []
  else []

pawn_move_capture : Board ColorPiece -> SquareIndex -> SquareIndex -> RankDelta -> Color -> List Move
pawn_move_capture board from to dir color =
  case board_get board to of
    Just (cap_color, cap_piece) ->
      if cap_color /= color
      then pawn_move board color from to dir (Just cap_piece)
      else []
    Nothing -> []

pawn_moves_captures : Board ColorPiece -> SquareIndex -> Color -> RankIndex -> List Move
pawn_moves_captures board (file, rank) color dir =
  pawn_move_capture board (file, rank) (file-1, rank+dir) dir color ++
  pawn_move_capture board (file, rank) (file+1, rank+dir) dir color

legal_moves_for_pawn : Board ColorPiece -> SquareIndex -> Color -> RankIndex -> RankDelta -> List Move
legal_moves_for_pawn board square color second dir =
  pawn_moves_forward board square color second dir ++
  pawn_moves_captures board square color dir

split : (a -> Bool) -> List a -> (List a, List a)
split p l =
  case l of
    [] -> ([], [])
    x :: xs -> if p x then let xs2 = split p xs in (x::fst xs2, snd xs2) else ([], l)

piece_move piece from captured to =
  { piece = piece, from = from, to = to, info = Nothing, check = False, captured = captured }

legal_moves_for_piece : Board ColorPiece -> SquareIndex -> ColorPiece -> List Move
legal_moves_for_piece board square (color, piece) =
  let pseudo_moves = withDefault [] (board_get (moves piece) square)
      pseudoToReal movelist =
        let (empty, nonEmpty) = split (isEmpty board) movelist
        in map (piece_move piece square Nothing) empty ++
           (case List.head nonEmpty of
              Just square_x ->
                case board_get board square_x of
                  Just (color_x, piece_x) -> if color == opposite color_x then [piece_move piece square (Just piece_x) square_x] else []
                  Nothing -> []
              Nothing -> [])
  in List.concat (map pseudoToReal pseudo_moves)

legal_moves_for : Board ColorPiece -> (SquareIndex, ColorPiece) -> List Move
legal_moves_for board (square, cp) =
  case cp of
    (White, P)     -> legal_moves_for_pawn board square White 1 1
    (Black, P)     -> legal_moves_for_pawn board square Black 6 -1
    (color, piece) -> legal_moves_for_piece board square cp

castling_moves : Game -> List Move
castling_moves game =
  let are_empty = map (flip Dict.member game.board) >> List.any identity >> not
      castles from to = { piece = K, from = from, to = to, info = Just Castled, check = False, captured = Nothing }
      check_castles can start mid finish = if can && are_empty [mid, finish] then [castles start finish] else []
  in case (game.to_move) of
     White ->
       check_castles game.castling.white.queenside e1 d1 c1 ++ check_castles game.castling.white.kingside e1 f1 g1
     Black ->
       check_castles game.castling.black.queenside e8 d8 c8 ++ check_castles game.castling.black.kingside e8 f8 g8

en_passant_moves : Game -> List Move
en_passant_moves game =
  let en_passant_move from to = { piece = P, from = from, to = to, info = Just EnPassant, check = False, captured = Just P }
  in case (game.to_move, game.en_passant) of
     (_, Nothing) -> []
     (White, Just (f,r)) ->
       if get (f-1, r-1) game.board == Just (White, P) then [en_passant_move (f-1, r-1) (f,r)] else [] ++
       if get (f+1, r-1) game.board == Just (White, P) then [en_passant_move (f+1, r-1) (f,r)] else []
     (Black, Just (f,r)) ->
       if get (f-1, r+1) game.board == Just (Black, P) then [en_passant_move (f-1, r+1) (f,r)] else [] ++
       if get (f+1, r+1) game.board == Just (Black, P) then [en_passant_move (f+1, r+1) (f,r)] else []


legal_moves : Game -> List Move
legal_moves game =
  (toIndexedList game.board |>
   List.filter (\(s, (c, p)) -> c == game.to_move) |>
   List.map (legal_moves_for game.board) |>
   List.concat) ++
  castling_moves game ++
  en_passant_moves game

castling_to_string : Castling -> String
castling_to_string c =
  let s p str = if p then str else ""
  in s c.white.kingside "K" ++ s c.white.queenside "Q" ++ s c.black.kingside "k" ++ s c.black.queenside "q"

en_passant_to_string : Maybe SquareIndex -> String
en_passant_to_string ep =
  case ep of
    Nothing -> "-"
    Just (square) -> square_to_string square

to_move_to_String : Color -> String
to_move_to_String to_move =
  case to_move of
    White -> "w"
    Black -> "b"

game_to_FEN: Game -> String
game_to_FEN { board, to_move, castling, en_passant, capture_or_pawn_move, move_number } =
  let rank_to_list rank = List.map (\file -> Dict.get (file,rank) board) [0..7]
      colorpiece_to_string (c,p) =
        let s = piece_to_string p in
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
  in String.join " " [board_to_FEN board, to_move_to_String to_move, castling_to_string castling, en_passant_to_string en_passant, toString capture_or_pawn_move, toString move_number]
