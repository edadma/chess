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
      "Knight near board edges" in /*withDebugLogging("Knight near board edges")*/ {
        // Test H8 corner knight first, with only one knight on the board
        val cornerBoard = Board.fromString(
          """
            |.  .  .  .  .  .  .  N
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
          """.stripMargin.trim,
        )

        logger.debug("Testing H8 knight moves")
        val h8Moves = cornerBoard.generateMoves(White).filter(_.from == H8).toSet
        logger.debug(s"Generated moves from H8: ${h8Moves.map(m => s"${toAlgebraic(m.from)}->${toAlgebraic(m.to)}")}")
        h8Moves.map(_.to) should contain only (F7, G6)

        // Test A4 edge knight separately
        val a4Board = Board.fromString(
          """
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |N  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
          """.stripMargin.trim,
        )

        logger.debug(s"Board string parsed, checking A4 position...")
        val a4Square = A4 // This should be 24
        logger.debug(s"A4 square number: $a4Square")
        logger.debug(s"Is knight present? ${a4Board.getPiece(a4Square)}")

        val a4Moves = a4Board.generateMoves(White).filter(_.from == a4Square).toSet
        logger.debug(s"Generated moves from A4: ${a4Moves.map(m => s"${toAlgebraic(m.from)}->${toAlgebraic(m.to)}")}")

        a4Moves.map(_.to) should contain only (B6, C5, C3, B2)

        // Test H1 corner knight
        val h1Board = Board.fromString(
          """
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  N
          """.stripMargin.trim,
        )

        val h1Moves = h1Board.generateMoves(White).filter(_.from == H1).toSet
        h1Moves.map(_.to) should contain only (F2, G3)

        // Test A1 corner knight
        val a1Board = Board.fromString(
          """
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |.  .  .  .  .  .  .  .
            |N  .  .  .  .  .  .  .
          """.stripMargin.trim,
        )

        val a1Moves = a1Board.generateMoves(White).filter(_.from == A1).toSet
        a1Moves.map(_.to) should contain only (B3, C2)
      }

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
      "allow castling kingside" in /*withDebugLogging("allow castling kingside")*/ {
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

        logger.debug(s"Castling rights: ${board.castlingRights}")
        logger.debug("Checking if squares F1 and G1 are empty")
        logger.debug(s"F1 occupied: ${board.getBit(board.occupied, F1)}")
        logger.debug(s"G1 occupied: ${board.getBit(board.occupied, G1)}")

        val moves = board.generateMoves(White).toList
        logger.debug(s"Generated moves: ${moves.mkString("\n  ", "\n  ", "")}")

        moves should contain(Move(E1, G1, WhiteKing, None, None, false, true))
      }

      "not allow castling through check" in {
        val board = Board.fromString("""
                                       |.  .  .  .  k  .  .  r
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  p  .
                                       |.  .  .  .  K  .  .  R
       """.stripMargin.trim)

        board.generateMoves(White).toList should not contain (Move(D1, F1, WhiteKing, isCastling = true))
      }
    }
  }
}
