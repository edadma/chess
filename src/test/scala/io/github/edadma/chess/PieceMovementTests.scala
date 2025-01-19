package io.github.edadma.chess

import Board.*

class PieceMovementTests extends ChessSpec {
  "Piece movement" - {
    "Pawn" - {
      val startingBoard = Board.fromString("""
                                             |.  .  .  .  .  .  .  .
                                             |.  .  .  .  .  .  .  .
                                             |.  .  .  .  .  .  .  .
                                             |.  .  .  .  .  .  .  .
                                             |.  .  .  .  .  .  .  .
                                             |.  .  .  .  .  .  .  .
                                             |.  .  P  .  .  .  .  .
                                             |.  .  .  .  .  .  .  .
     """.stripMargin.trim)

      "allow single forward move" in {
        startingBoard.generateMoves(White).toList should contain(Move(C2, C3, WhitePawn))
      }

      "allow double move from starting position" in {
        startingBoard.generateMoves(White).toList should contain(Move(C2, C4, WhitePawn))
      }

      "allow diagonal capture" in {
        val boardWithCapture = Board.fromString("""
                                                  |.  .  .  .  .  .  .  .
                                                  |.  .  .  .  .  .  .  .
                                                  |.  p  .  p  .  .  .  .
                                                  |.  .  P  .  .  .  .  .
                                                  |.  .  .  .  .  .  .  .
                                                  |.  .  .  .  .  .  .  .
                                                  |.  .  .  .  .  .  .  .
                                                  |.  .  .  .  .  .  .  .
       """.stripMargin.trim)
        val moves = boardWithCapture.generateMoves(White).toList
        moves should contain(Move(C5, B6, WhitePawn, Some(BlackPawn)))
        moves should contain(Move(C5, D6, WhitePawn, Some(BlackPawn)))
      }

      "allow en passant capture" in {
        // Setup board with last move being black pawn double advance
        val board = Board.fromString("""
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  p  P  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
       """.stripMargin.trim)
        val lastMove          = Move(C7, C5, BlackPawn)
        val boardWithLastMove = board.copy(lastMove = Some(lastMove), enPassantSquare = Some(C6))

        boardWithLastMove.generateMoves(White).toList should contain(Move(
          D5,
          C6,
          WhitePawn,
          Some(BlackPawn),
          isEnPassant = true,
        ))
      }
    }

    "Knight" - {
      "generate all valid L-shaped moves" in {
        val board = Board.fromString("""
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  N  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
       """.stripMargin.trim)

        val moves = board.generateMoves(White).toSet
        moves should contain(Move(D4, 10, WhiteKnight))
        moves should contain(Move(D4, 12, WhiteKnight))
        moves should contain(Move(D4, 17, WhiteKnight))
        moves should contain(Move(D4, 21, WhiteKnight))
        moves should contain(Move(D4, 33, WhiteKnight))
        moves should contain(Move(D4, 37, WhiteKnight))
        moves should contain(Move(D4, 42, WhiteKnight))
        moves should contain(Move(D4, 44, WhiteKnight))
      }
    }

    "Rook" - {
      "generate straight line moves" in {
        val board = Board.fromString("""
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  p  .  .  .  .
                                       |.  .  .  R  .  .  .  .
                                       |.  .  .  P  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
       """.stripMargin.trim)

        val moves = board.generateMoves(White).filter(_.piece == WhiteRook).toSet
        moves should contain(Move(D5, D6, WhiteRook, Some(BlackPawn))) // Capture up
        moves should contain(Move(D5, C5, WhiteRook))                  // Left
        moves should contain(Move(D5, B5, WhiteRook))                  // Left
        moves should contain(Move(D5, A5, WhiteRook))                  // Left
        moves should contain(Move(D5, E5, WhiteRook))                  // Right
        moves should contain(Move(D5, F5, WhiteRook))                  // Right
        moves should contain(Move(D5, G5, WhiteRook))                  // Right
        moves should contain(Move(D5, H5, WhiteRook))                  // Right
      }
    }

    "King" - {
      "allow castling kingside" in withDebugLogging("allow castling kingside") {
        val board = Board.fromString("""
                                       |.  .  .  .  k  .  .  r
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  K  .  .  R
       """.stripMargin.trim)

        board.generateMoves(White).toList should contain(Move(E1, G1, WhiteKing, isCastling = true))
      }

//      "not allow castling through check" in {
//        val board = Board.fromString("""
//                                       |.  .  .  .  k  .  .  r
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  p  .
//                                       |.  .  .  .  K  .  .  R
//       """.stripMargin.trim)
//
//        board.generateMoves(White).toList should not contain (Move(4, 6, WhiteKing, isCastling = true))
//      }
    }
  }
}
