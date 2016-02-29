# chess
A simple Chess UI written in Elm.

I wanted to learn Elm and I've been meaning to write a chess UI/program
for a while some this is my attempt to kill two birds with the one stone.

To simplify the UI, I leverage the unicode chess pieces.

The first goal is to build a UI that allows you to play through any legal game.

Done:
1. piece moves (and captures)
2. pawns moves (and captures)
3. double pawn moves
4. en passant
5. castling
6. check detection

To-do:
1. handle getting out of check
2. enforce kings can't meet
3. checks that result from castling
4. can't castle through check
