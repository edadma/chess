package io.github.edadma.chess

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import Board.*

class PieceMovementTests extends AnyFreeSpec with Matchers {
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

//      "allow double move from starting position" in {
//        startingBoard.generateMoves(White).toList should contain(Move(18, 34, WhitePawn))
//      }
//
//      "allow diagonal capture" in {
//        val boardWithCapture = Board.fromString("""
//                                                  |.  .  .  .  .  .  .  .
//                                                  |.  .  .  .  .  .  .  .
//                                                  |.  p  .  p  .  .  .  .
//                                                  |.  .  P  .  .  .  .  .
//                                                  |.  .  .  .  .  .  .  .
//                                                  |.  .  .  .  .  .  .  .
//                                                  |.  .  .  .  .  .  .  .
//                                                  |.  .  .  .  .  .  .  .
//       """.stripMargin.trim)
//        val moves = boardWithCapture.generateMoves(White).toList
//        moves should contain(Move(18, 9, WhitePawn, Some(BlackPawn)))
//        moves should contain(Move(18, 11, WhitePawn, Some(BlackPawn)))
//      }
//
//      "allow en passant capture" in {
//        // Setup board with last move being black pawn double advance
//        val board = Board.fromString("""
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  p  P  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//       """.stripMargin.trim)
//        val lastMove          = Move(30, 14, BlackPawn)
//        val boardWithLastMove = board.copy(lastMove = Some(lastMove), enPassantSquare = Some(22))
//
//        boardWithLastMove.generateMoves(White).toList should contain(Move(
//          23,
//          22,
//          WhitePawn,
//          Some(BlackPawn),
//          isEnPassant = true,
//        ))
//      }
    }

//    "Knight" - {
//      "generate all valid L-shaped moves" in {
//        val board = Board.fromString("""
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  N  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//       """.stripMargin.trim)
//
//        val moves = board.generateMoves(White).toSet
//        moves should contain(Move(27, 10, WhiteKnight))
//        moves should contain(Move(27, 12, WhiteKnight))
//        moves should contain(Move(27, 17, WhiteKnight))
//        moves should contain(Move(27, 21, WhiteKnight))
//        moves should contain(Move(27, 33, WhiteKnight))
//        moves should contain(Move(27, 37, WhiteKnight))
//        moves should contain(Move(27, 42, WhiteKnight))
//        moves should contain(Move(27, 44, WhiteKnight))
//      }
//    }

//    "Rook" - {
//      "generate straight line moves" in {
//        val board = Board.fromString("""
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  p  .  .  .  .
//                                       |.  .  .  R  .  .  .  .
//                                       |.  .  .  P  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//       """.stripMargin.trim)
//
//        val moves = board.generateMoves(White).filter(_.piece == WhiteRook).toSet
//        moves should contain(Move(27, 19, WhiteRook, Some(BlackPawn))) // Capture up
//        moves should contain(Move(27, 24, WhiteRook))                  // Left
//        moves should contain(Move(27, 25, WhiteRook))                  // Left
//        moves should contain(Move(27, 26, WhiteRook))                  // Left
//        moves should contain(Move(27, 28, WhiteRook))                  // Right
//        moves should contain(Move(27, 29, WhiteRook))                  // Right
//        moves should contain(Move(27, 30, WhiteRook))                  // Right
//      }
//    }

//    "King" - {
//      "allow castling kingside" in {
//        val board = Board.fromString("""
//                                       |.  .  .  .  k  .  .  r
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  K  .  .  R
//       """.stripMargin.trim)
//
//        board.generateMoves(White).toList should contain(Move(4, 6, WhiteKing, isCastling = true))
//      }
//
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
//    }
  }
}
