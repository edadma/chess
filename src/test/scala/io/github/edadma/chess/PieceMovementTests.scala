//package io.github.edadma.chess
//
//import Board.*
//
//import pprint.pprintln
//
//class PieceMovementTests extends ChessSpec {
//  "Queen" - {
//    "basic moves" - {
//      "basic queen capture along file" in withDebugLogging("queen capture test") {
//        val board = Board.fromString(
//          """
//            |.  .  .  r  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  Q  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .""".stripMargin.trim,
//        )
//
//        val moves = board.generateMoves(White).filter(_.piece == WhiteQueen).toSet
//
//        // Test specific capture move
//        moves.find(_.to == D8) match {
//          case Some(move) =>
//            move.capture shouldBe Some(BlackRook)
//          case None =>
//            fail("Queen should be able to capture rook on d8")
//        }
//
//        // Also verify other vertical moves
//        moves.map(_.to) should contain allOf (
//          D7, // Square above queen
//          D8, // Rook's square
//        )
//      }
//
//      "move in all directions on empty board" in {
//        val board = Board.fromString("""
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  Q  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .""".stripMargin.trim)
//
//        val moves = board.generateMoves(White).filter(_.piece == WhiteQueen).map(_.to).toSet
//
//        // Horizontal moves
//        moves should contain allOf (A5, B5, C5, E5, F5, G5, H5)
//        // Vertical moves
//        moves should contain allOf (D1, D2, D3, D4, D6, D7, D8)
//        // Diagonal moves
//        moves should contain allOf (A2, B3, C4, E6, F7, G8)     // Bottom-left to top-right
//        moves should contain allOf (A8, B7, C6, E4, F3, G2, H1) // Top-left to bottom-right
//      }
//
//      "capture enemy pieces" in {
//        val board = Board.fromString("""
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  p  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  Q  .  .  .  .
//                                       |.  .  .  .  p  .  .  .
//                                       |.  .  .  .  .  n  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .""".stripMargin.trim)
//
//        val moves    = board.generateMoves(White).filter(_.piece == WhiteQueen).toSet
//        val captures = moves.map(m => (m.to, m.capture))
//
//        // Should be able to capture all three black pieces
//        captures should contain allOf (
//          (D7, Some(BlackPawn)), // Vertical capture
//          (E4, Some(BlackPawn)), // Diagonal capture
//        )
//        captures should not contain (F3, Some(BlackKnight)) // Diagonal capture
//      }
//
//      "be blocked by friendly pieces" in {
//        val board = Board.fromString("""
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  P  .  .  .  .
//                                       |.  .  .  Q  N  .  .  .
//                                       |.  .  .  B  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .""".stripMargin.trim)
//
//        val moves = board.generateMoves(White).filter(_.piece == WhiteQueen).map(_.to).toSet
//
//        // Should not be able to move through or capture friendly pieces
//        moves should not contain allOf(D6, D4, E4)
//        // Should be able to move in unblocked directions
//        moves should contain allOf (C5, C4, C6, E4, E6)
//      }
//    }
//
//    "complex scenarios" - {
//      "respect pin to king" in {
//        val board = Board.fromString("""
//                                       |.  .  .  r  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  Q  .  .  .  .
//                                       |.  .  .  K  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .""".stripMargin.trim)
//
//        val moves = board.generateMoves(White).filter(_.piece == WhiteQueen).map(_.to).toSet
//
//        // Queen is pinned vertically - can only move up/down
//        moves should contain only (D7, D8)
//      }
//
////      "prevent moves that would leave king in check" in {
////        val board = Board.fromString("""
////                                       |.  .  .  .  .  .  .  .
////                                       |.  .  .  .  .  .  .  .
////                                       |.  .  .  r  .  .  .  .
////                                       |.  .  Q  K  .  .  .  .
////                                       |.  .  .  .  .  .  .  .
////                                       |.  .  .  .  .  .  .  .""".stripMargin.trim)
////
////        val moves = board.generateMoves(White).filter(_.piece == WhiteQueen).map(_.to).toSet
////
////        // Queen must block check or capture rook
////        moves should contain only (D6)
////      }
//
////      "handle multiple threats" in {
////        val board = Board.fromString("""
////                                       |.  .  .  r  .  .  .  .
////                                       |.  .  .  .  .  .  .  .
////                                       |.  .  .  Q  .  b  .  .
////                                       |.  .  .  K  .  .  .  .
////                                       |.  .  .  .  .  .  .  .
////                                       |.  .  .  .  .  .  .  .""".stripMargin.trim)
////
////        val moves = board.generateMoves(White).filter(_.piece == WhiteQueen).map(_.to).toSet
////
////        // Queen must deal with both rook and bishop threats
////        moves should contain only (D7, D8) // Can only move to block/capture rook
////      }
//    }
//  }
//
//  "Queenside castling with blocked squares" in {
//    val board = Board.fromString(
//      """
//        |.  .  .  .  k  .  .  .
//        |.  .  .  .  .  .  .  .
//        |.  .  .  .  .  .  .  .
//        |.  .  .  .  .  .  .  .
//        |.  .  .  .  .  .  .  .
//        |.  .  .  .  .  .  .  .
//        |.  .  .  .  .  .  .  .
//        |R  .  N  .  K  .  .  R
//      """.stripMargin.trim,
//    )
//
//    // Knight blocks queenside castling
//    val moves = board.generateMoves(White).filter(_.isCastling).toList
//    moves.map(_.to) should contain only (G1) // Only kingside should be possible
//  }
//
//  "Piece movement" - {
//    "Pawn promotion" in {
//      val board = Board.fromString(
//        """
//          |.  .  .  .  .  .  .  .
//          |.  .  P  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//        """.stripMargin.trim,
//      )
//
//      val moves = board.generateMoves(White).filter(_.from == C7).toList
//      moves.map(_.promotion) should contain only (
//        Some(WhiteQueen),
//        Some(WhiteRook),
//        Some(WhiteBishop),
//        Some(WhiteKnight),
//      )
//    }
//
//    "Pawn blocked movement" in {
//      val board = Board.fromString(
//        """
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  p  .  .  .  .  .
//          |.  .  P  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//        """.stripMargin.trim,
//      )
//
//      val moves = board.generateMoves(White).filter(_.from == C4).toList
//      moves shouldBe empty // Pawn should be blocked
//    }
//
//    "Bishop movement and blocking" in /*withDebugLogging("Bishop movement and blocking")*/ {
//      val board = Board.fromString(
//        """
//          |.  .  .  .  .  .  .  .
//          |.  .  .  p  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  B  .  .  .  .
//          |.  .  .  .  P  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//        """.stripMargin.trim,
//      )
//
//      logger.debug("Testing bishop moves")
//      val bishopSquare = D5
//      logger.debug(s"Bishop is on square ${toAlgebraic(bishopSquare)}")
//
//      val moves = board.generateMoves(White).filter(_.piece == WhiteBishop).toSet
//      logger.debug(s"Generated moves: ${moves.map(m => s"${toAlgebraic(m.from)}->${toAlgebraic(m.to)}")}")
//
//      val targetSquares = moves.map(_.to)
//      logger.debug(s"Target squares: ${targetSquares.map(toAlgebraic)}")
//
//      moves.map(_.to) should contain only (
//        C6,
//        B7,
//        A8,
//        E6,
//        F7,
//        G8,
//        C4,
//        B3,
//        A2,
//      )
//    }
//
//    "Pawn" - {
//      val startingBoard = Board.fromString("""
//                                             |.  .  .  .  .  .  .  .
//                                             |.  .  .  .  .  .  .  .
//                                             |.  .  .  .  .  .  .  .
//                                             |.  .  .  .  .  .  .  .
//                                             |.  .  .  .  .  .  .  .
//                                             |.  .  .  .  .  .  .  .
//                                             |.  .  P  .  .  .  .  .
//                                             |.  .  .  .  .  .  .  .
//     """.stripMargin.trim)
//
//      "allow single forward move" in {
//        startingBoard.generateMoves(White).toList should contain(Move(C2, C3, WhitePawn))
//      }
//
//      "allow double move from starting position" in {
//        startingBoard.generateMoves(White).toList should contain(Move(C2, C4, WhitePawn))
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
//        val movesWhite = boardWithCapture.generateMoves(White).toList
//        movesWhite should contain(Move(C5, B6, WhitePawn, Some(BlackPawn)))
//        movesWhite should contain(Move(C5, D6, WhitePawn, Some(BlackPawn)))
//        val movesBlack = boardWithCapture.generateMoves(Black).toList
//        movesBlack should contain(Move(B6, C5, BlackPawn, Some(WhitePawn)))
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
//        val lastMove          = Move(C7, C5, BlackPawn)
//        val boardWithLastMove = board.copy(lastMove = Some(lastMove), enPassantSquare = Some(C6))
//
//        boardWithLastMove.generateMoves(White).toList should contain(Move(
//          D5,
//          C6,
//          WhitePawn,
//          Some(BlackPawn),
//          isEnPassant = true,
//        ))
//      }
//
//      "correctly detect pawn attacks" in /*withDebugLogging("pawn attack test")*/ {
//        val board = Board.fromString(
//          """
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  p  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  p  .
//            |.  .  .  .  .  .  .  .
//          """.stripMargin.trim,
//        )
//
//        logger.debug("Testing if d3 is attacked by black pawn")
//        board.isSquareAttacked(D3, White) shouldBe true // d3 should be under attack
//
//        logger.debug("Testing if f3 is attacked by black pawn")
//        board.isSquareAttacked(F3, White) shouldBe true // f3 should be under attack
//
//        logger.debug("Testing if e3 is not attacked by black pawn")
//        board.isSquareAttacked(E3, White) shouldBe false // e3 should not be under attack
//
//        board.isSquareAttacked(F1, White) shouldBe true
//      }
//    }
//
//    "Knight" - {
//      "Knight near board edges" in /*withDebugLogging("Knight near board edges")*/ {
//        // Test H8 corner knight first, with only one knight on the board
//        val cornerBoard = Board.fromString(
//          """
//            |.  .  .  .  .  .  .  N
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//          """.stripMargin.trim,
//        )
//
//        logger.debug("Testing H8 knight moves")
//        val h8Moves = cornerBoard.generateMoves(White).filter(_.from == H8).toSet
//        logger.debug(s"Generated moves from H8: ${h8Moves.map(m => s"${toAlgebraic(m.from)}->${toAlgebraic(m.to)}")}")
//        h8Moves.map(_.to) should contain only (F7, G6)
//
//        // Test A4 edge knight separately
//        val a4Board = Board.fromString(
//          """
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |N  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//          """.stripMargin.trim,
//        )
//
//        logger.debug(s"Board string parsed, checking A4 position...")
//        val a4Square = A4 // This should be 24
//        logger.debug(s"A4 square number: $a4Square")
//        logger.debug(s"Is knight present? ${a4Board.getPiece(a4Square)}")
//
//        val a4Moves = a4Board.generateMoves(White).filter(_.from == a4Square).toSet
//        logger.debug(s"Generated moves from A4: ${a4Moves.map(m => s"${toAlgebraic(m.from)}->${toAlgebraic(m.to)}")}")
//
//        a4Moves.map(_.to) should contain only (B6, C5, C3, B2)
//
//        // Test H1 corner knight
//        val h1Board = Board.fromString(
//          """
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  N
//          """.stripMargin.trim,
//        )
//
//        val h1Moves = h1Board.generateMoves(White).filter(_.from == H1).toSet
//        h1Moves.map(_.to) should contain only (F2, G3)
//
//        // Test A1 corner knight
//        val a1Board = Board.fromString(
//          """
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |N  .  .  .  .  .  .  .
//          """.stripMargin.trim,
//        )
//
//        val a1Moves = a1Board.generateMoves(White).filter(_.from == A1).toSet
//        a1Moves.map(_.to) should contain only (B3, C2)
//      }
//
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
//        moves should contain(Move(D4, 10, WhiteKnight))
//        moves should contain(Move(D4, 12, WhiteKnight))
//        moves should contain(Move(D4, 17, WhiteKnight))
//        moves should contain(Move(D4, 21, WhiteKnight))
//        moves should contain(Move(D4, 33, WhiteKnight))
//        moves should contain(Move(D4, 37, WhiteKnight))
//        moves should contain(Move(D4, 42, WhiteKnight))
//        moves should contain(Move(D4, 44, WhiteKnight))
//      }
//    }
//
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
//        moves should contain(Move(D5, D6, WhiteRook, Some(BlackPawn))) // Capture up
//        moves should contain(Move(D5, C5, WhiteRook))                  // Left
//        moves should contain(Move(D5, B5, WhiteRook))                  // Left
//        moves should contain(Move(D5, A5, WhiteRook))                  // Left
//        moves should contain(Move(D5, E5, WhiteRook))                  // Right
//        moves should contain(Move(D5, F5, WhiteRook))                  // Right
//        moves should contain(Move(D5, G5, WhiteRook))                  // Right
//        moves should contain(Move(D5, H5, WhiteRook))                  // Right
//      }
//    }
//
//    "King" - {
//      "allow castling kingside" in /*withDebugLogging("allow castling kingside")*/ {
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
//        logger.debug(s"Castling rights: ${board.castlingRights}")
//        logger.debug("Checking if squares F1 and G1 are empty")
//        logger.debug(s"F1 occupied: ${board.getBit(board.occupied, F1)}")
//        logger.debug(s"G1 occupied: ${board.getBit(board.occupied, G1)}")
//
//        val moves = board.generateMoves(White).toList
//        logger.debug(s"Generated moves: ${moves.mkString("\n  ", "\n  ", "")}")
//
//        moves should contain(Move(E1, G1, WhiteKing, None, None, false, true))
//      }
//
//      "not allow castling through threatened square (pawn)" in /*withDebugLogging(
//        "not allow castling through threatened square (pawn)",
//      )*/ {
//        val board = Board.fromString("""
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  p  .
//                                       |.  .  .  .  K  .  .  R
//       """.stripMargin.trim)
//
//        board.canCastleKingside(White) shouldBe false
//
//        // Test the F1 square attack explicitly
//        val f1IsAttacked = board.isSquareAttacked(Board.F1, White)
//        logger.debug(s"Is F1 square (index ${Board.F1}) attacked? $f1IsAttacked")
//
//        // Let's also check the G2 pawn's position
//        val g2HasPawn = board.getPiece(Board.G2)
//        logger.debug(s"Piece on G2 (index ${Board.G2}): $g2HasPawn")
//
//        val castlingMoves = board.generateMoves(White).filter(_.isCastling).toList
//        logger.debug(s"Generated castling moves: $castlingMoves")
//
//        castlingMoves should contain only Move(4, 2, WhiteKing, None, None, false, true)
//      }
//
//      "not allow castling through threatened square (bishop)" in {
//        val board = Board.fromString("""
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  .  .
//                                       |.  .  .  .  .  .  b  .
//                                       |.  .  .  .  K  .  .  R
//       """.stripMargin.trim)
//
//        board.generateMoves(White).filter(_.isCastling).toList should contain only Move(
//          4,
//          2,
//          WhiteKing,
//          None,
//          None,
//          false,
//          true,
//        )
//      }
//
//      "not allow castling through check" in /*withDebugLogging("not allow castling through check")*/ {
//        val board = Board.fromString(
//          """
//            |.  .  .  .  k  .  .  r
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  n  .  .  .  .
//            |.  .  .  .  .  .  .  .
//            |.  .  .  .  K  .  .  R
//          """.stripMargin.trim,
//        )
//
//        board.generateMoves(White).filter(_.isCastling) shouldBe empty
//      }
//    }
//  }
//}
