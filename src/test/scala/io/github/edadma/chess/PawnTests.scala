package io.github.edadma.chess

class PawnTests extends ChessSpec {
  "basic moves" - {
    "initial two square advance" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  P  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      val moves = board.getMoves(White).map(m => (m.fromIndex, m.toIndex)).toSet
      moves should contain(fromAlgebraic("d2") -> fromAlgebraic("d3"))
      moves should contain(fromAlgebraic("d2") -> fromAlgebraic("d4"))
    }

    "single square advance after initial move" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  P  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      val moves = board.getMoves(White).map(m => (m.fromIndex, m.toIndex)).toSet
      moves should contain only (fromAlgebraic("d4") -> fromAlgebraic("d5"))
    }
  }

  "captures" - {
    "diagonal captures" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  p  .  p  .  .  .
                                     |.  .  .  P  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      val moves = board.getMoves(White).map(m => (m.fromIndex, m.toIndex)).toSet
      moves should contain allOf (
        fromAlgebraic("d4") -> fromAlgebraic("c5"),
        fromAlgebraic("d4") -> fromAlgebraic("e5"),
        fromAlgebraic("d4") -> fromAlgebraic("d5"),
      )
    }

    "en passant capture" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  P  p  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      val lastMove          = Move("e7", "e5", BlackPawn, MoveType.NORMAL)
      val boardWithLastMove = board.copy(lastMove = Some(lastMove))

      val moves = boardWithLastMove.getMoves(White).filter(_.moveType == MoveType.EN_PASSANT)
      moves.map(m => (m.fromIndex, m.toIndex)).toSet should contain only
        (fromAlgebraic("d5") -> fromAlgebraic("e6"))
    }
  }

  "promotion" - {
    "should offer promotion options" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  P  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      val moves = board.getMoves(White).toSet
      moves should contain allOf (
        Move("d7", "d8", WhitePawn, MoveType.NORMAL, Some(WhiteQueen)),
        Move("d7", "d8", WhitePawn, MoveType.NORMAL, Some(WhiteRook)),
        Move("d7", "d8", WhitePawn, MoveType.NORMAL, Some(WhiteBishop)),
        Move("d7", "d8", WhitePawn, MoveType.NORMAL, Some(WhiteKnight)),
      )
    }
  }

  "blocked movements" - {
    "blocked by enemy piece" in {
      val board = Board.fromString(
        """
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  p  .  .  .  .
          |.  .  .  P  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |""".stripMargin,
      )

      board.getMoves(White).filter(_.piece == WhitePawn) shouldBe empty
    }

    "blocked by friendly piece" in {
      val board = Board.fromString(
        """
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  N  .  .  .  .
          |.  .  .  P  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |""".stripMargin,
      )

      board.getMoves(White).filter(_.piece == WhitePawn) shouldBe empty
    }

    "two-square advance blocked by enemy piece" in {
      val board = Board.fromString(
        """
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  p  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  P  .  .  .  .
          |.  .  .  .  .  .  .  .
          |""".stripMargin,
      )

      val moves = board.getMoves(White).filter(_.piece == WhitePawn).map(m => (m.fromIndex, m.toIndex)).toSet
      moves should contain only (fromAlgebraic("d2") -> fromAlgebraic("d3"))
    }
  }

  "capture with promotion" - {
    "diagonal capture with promotion options" in {
      val board = Board.fromString(
        """
          |.  .  .  n  b  n  .  .
          |.  .  .  .  P  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |""".stripMargin,
      )

      val moves = board.getMoves(White).filter(_.piece == WhitePawn).toSet

      // Left capture promotion
      moves should contain(Move("e7", "d8", WhitePawn, MoveType.NORMAL, Some(WhiteQueen)))
      moves should contain(Move("e7", "d8", WhitePawn, MoveType.NORMAL, Some(WhiteRook)))
      moves should contain(Move("e7", "d8", WhitePawn, MoveType.NORMAL, Some(WhiteBishop)))
      moves should contain(Move("e7", "d8", WhitePawn, MoveType.NORMAL, Some(WhiteKnight)))

      // Right capture promotion
      moves should contain(Move("e7", "f8", WhitePawn, MoveType.NORMAL, Some(WhiteQueen)))
      moves should contain(Move("e7", "f8", WhitePawn, MoveType.NORMAL, Some(WhiteRook)))
      moves should contain(Move("e7", "f8", WhitePawn, MoveType.NORMAL, Some(WhiteBishop)))
      moves should contain(Move("e7", "f8", WhitePawn, MoveType.NORMAL, Some(WhiteKnight)))

      moves.size shouldBe 8
    }
  }
}
