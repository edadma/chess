package io.github.edadma.chess

class KnightTests extends ChessSpec {
  "inner" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  n  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    val attackedSquares = List(
      (2, 7),
      (1, 6),
      (1, 4),
      (2, 3),
      (4, 3),
      (5, 4),
      (5, 6),
      (4, 7),
    ).map { case (f, r) => r * 8 + f }

    attackedSquares.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "center square should not be attacked" in {
      assert(!board.isSquareAttacked(27, Black))
    }
  }

  "blocked by friendly piece" in {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  n  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  p  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    assert(!board.isSquareAttacked(fromAlgebraic("e4"), Black)) // Square with black pawn
  }

  "corner" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  n
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    val expectedAttacks = List(
      (5, 6),
      (6, 5),
    ).map { case (f, r) => r * 8 + f }

    expectedAttacks.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }
  }
}
