package io.github.edadma.chess

class BishopTests extends ChessSpec {
  "inner" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  b  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    val attackedSquares = List(
      (1, 7), // up-left diagonal
      (2, 6),
      (4, 6), // up-right diagonal
      (5, 7),
      (2, 4), // down-left diagonal
      (1, 3),
      (4, 4), // down-right diagonal
      (5, 3),
      (6, 2),
      (7, 1),
    ).map { case (f, r) => r * 8 + f }

    attackedSquares.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "center square should not be attacked" in {
      assert(!board.isSquareAttacked(27, Black))
    }

    "should be 11 squares" in {
      board.getMoves(Black).length shouldBe 11
    }
  }

  "blocked by friendly piece" in {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  b  .  .  .  .
        |.  .  .  .  P  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    assert(board.isSquareAttacked(fromAlgebraic("e5"), Black))  // Square with white pawn
    assert(!board.isSquareAttacked(fromAlgebraic("f4"), Black)) // Square beyond white pawn

    board.getMoves(Black).count(_.piece == BlackBishop) shouldBe 8
  }

  "corner" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  b
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
      (6, 6),
      (5, 5),
      (4, 4),
      (3, 3),
      (2, 2),
      (1, 1),
      (0, 0),
    ).map { case (f, r) => r * 8 + f }

    expectedAttacks.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "should be 7 squares" in {
      board.getMoves(Black).length shouldBe 7
    }
  }

  "side" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  b
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    val expectedAttacks = List(
      (6, 4),
      (5, 3),
      (4, 2),
      (3, 1),
      (2, 0),
      (6, 6),
      (5, 7),
    ).map { case (f, r) => r * 8 + f }

    expectedAttacks.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "should be 7 squares" in {
      board.getMoves(Black).length shouldBe 7
    }
  }
}
