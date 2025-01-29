package io.github.edadma.chess

class CheckMoveTests extends ChessSpec {
  "moving into check" - {
    "should not allow king to move into rook's attack line" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  k  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  R  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      // Try to move king to f6, which would be in rook's attack line
      val moves = board.getMoves(Black).toList
      moves.exists(m =>
        m.fromIndex == fromAlgebraic("f6") &&
          m.toIndex == fromAlgebraic("g6"),
      ) shouldBe false
    }

    "should not allow king to move into bishop's attack line" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  k  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  B
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      // Try to move king to f6, which would be in bishop's attack line
      val moves = board.getMoves(Black).toList
      moves.exists(m =>
        m.fromIndex == fromAlgebraic("e6") &&
          m.toIndex == fromAlgebraic("f6"),
      ) shouldBe false
    }

    "should not allow king to move into knight's attack squares" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  k  .  .  .  .
                                     |.  .  .  .  .  .  N  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      // Try to move king to f6, which would be in knight's attack range
      val moves = board.getMoves(Black).toList
      moves.exists(m =>
        m.fromIndex == fromAlgebraic("d6") &&
          m.toIndex == fromAlgebraic("e6"),
      ) shouldBe false
    }
  }
}
