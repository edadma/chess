package io.github.edadma.chess

class CastlingTests extends ChessSpec {
  "castling rights" - {
    "should be lost when king moves" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      val kingMove = Move("e1", "e2", WhiteKing, MoveType.NORMAL)
      val newBoard = board.applyMove(kingMove)

      assert(!newBoard.canCastleKingside(White))
      assert(!newBoard.canCastleQueenside(White))
      assert(newBoard.canCastleKingside(Black))
      assert(newBoard.canCastleQueenside(Black))
    }

    "should be lost on kingside when kingside rook moves" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      val rookMove = Move("h1", "h2", WhiteRook, MoveType.NORMAL)
      val newBoard = board.applyMove(rookMove)

      assert(!newBoard.canCastleKingside(White))
      assert(newBoard.canCastleQueenside(White))
      assert(newBoard.canCastleKingside(Black))
      assert(newBoard.canCastleQueenside(Black))
    }

    "should be lost on queenside when queenside rook moves" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      val rookMove = Move("a1", "a2", WhiteRook, MoveType.NORMAL)
      val newBoard = board.applyMove(rookMove)

      assert(newBoard.canCastleKingside(White))
      assert(!newBoard.canCastleQueenside(White))
      assert(newBoard.canCastleKingside(Black))
      assert(newBoard.canCastleQueenside(Black))
    }
  }

  "kingside castling" - {
    "should be legal in initial position" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      val moves = board.getMoves(White).filter(_.moveType == MoveType.CASTLE_KINGSIDE).toList
      moves.length shouldBe 1
      moves.head.fromIndex shouldBe fromAlgebraic("e1")
      moves.head.toIndex shouldBe fromAlgebraic("g1")
    }

    "should be illegal when pieces are between king and rook" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  n  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |R  .  .  .  K  .  N  R
                                     |""".stripMargin)

      board.getMoves(White).filter(_.moveType == MoveType.CASTLE_KINGSIDE) shouldBe empty
      board.getMoves(Black).filter(_.moveType == MoveType.CASTLE_KINGSIDE) shouldBe empty
    }

    "should be illegal when king passes through check" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  r  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      board.getMoves(White).filter(_.moveType == MoveType.CASTLE_KINGSIDE) shouldBe empty
    }

    "should be illegal when king is in check" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  r  .  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      board.getMoves(White).filter(_.moveType == MoveType.CASTLE_KINGSIDE) shouldBe empty
    }
  }

  "queenside castling" - {
    "should be legal in initial position" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      val moves = board.getMoves(White).filter(_.moveType == MoveType.CASTLE_QUEENSIDE).toList
      moves.length shouldBe 1
      moves.head.fromIndex shouldBe fromAlgebraic("e1")
      moves.head.toIndex shouldBe fromAlgebraic("c1")
    }

    "should be illegal when pieces are between king and rook" in {
      val board = Board.fromString("""
                                     |r  .  .  b  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |R  .  .  B  K  .  .  R
                                     |""".stripMargin)

      board.getMoves(White).filter(_.moveType == MoveType.CASTLE_QUEENSIDE) shouldBe empty
      board.getMoves(Black).filter(_.moveType == MoveType.CASTLE_QUEENSIDE) shouldBe empty
    }

    "should be illegal when king passes through check" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  r  .  .  .  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      board.getMoves(White).filter(_.moveType == MoveType.CASTLE_QUEENSIDE) shouldBe empty
    }
  }

  "castling execution" - {
    "kingside castling should move both king and rook" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      val move     = Move("e1", "g1", WhiteKing, MoveType.CASTLE_KINGSIDE)
      val newBoard = board.applyMove(move)

      newBoard.getPiece(fromAlgebraic("g1")).get shouldBe WhiteKing
      newBoard.getPiece(fromAlgebraic("f1")).get shouldBe WhiteRook
      newBoard.getPiece(fromAlgebraic("e1")) shouldBe None
      newBoard.getPiece(fromAlgebraic("h1")) shouldBe None
    }

    "queenside castling should move both king and rook" in {
      val board = Board.fromString("""
                                     |r  .  .  .  k  .  .  r
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |R  .  .  .  K  .  .  R
                                     |""".stripMargin)

      val move     = Move("e1", "c1", WhiteKing, MoveType.CASTLE_QUEENSIDE)
      val newBoard = board.applyMove(move)

      newBoard.getPiece(fromAlgebraic("c1")).get shouldBe WhiteKing
      newBoard.getPiece(fromAlgebraic("d1")).get shouldBe WhiteRook
      newBoard.getPiece(fromAlgebraic("e1")) shouldBe None
      newBoard.getPiece(fromAlgebraic("a1")) shouldBe None
    }
  }
}
