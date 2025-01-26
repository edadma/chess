package io.github.edadma.chess

class KingTests extends ChessSpec {
  "inner" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  k  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    val attackedSquares = List(
      (2, 6),
      (3, 6),
      (4, 6),
      (2, 5),
      (4, 5),
      (2, 4),
      (3, 4),
      (4, 4),
    ).map { case (f, r) => r * 8 + f }

    attackedSquares.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "center square should not be attacked" in {
      assert(!board.isSquareAttacked(27, Black))
    }

    "should be 8 squares" in {
      board.getMoves(Black).length shouldBe 8
    }
  }

  "blocked by friendly piece" in {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  k  .  .  .  .
        |.  .  .  .  p  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    assert(!board.isSquareAttacked(fromAlgebraic("e5"), Black))

    board.getMoves(Black).length shouldBe 7
  }

  "corner" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  k
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
      (6, 7),
      (6, 6),
      (7, 6),
    ).map { case (f, r) => r * 8 + f }

    expectedAttacks.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "should be 3 squares" in {
      board.getMoves(Black).length shouldBe 3
    }
  }

  "side" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  k
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    val expectedAttacks = List(
      (6, 6),
      (7, 6),
      (6, 5),
      (6, 4),
      (7, 4),
    ).map { case (f, r) => r * 8 + f }

    expectedAttacks.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "should be 5 squares" in {
      board.getMoves(Black).length shouldBe 5
    }
  }
}
