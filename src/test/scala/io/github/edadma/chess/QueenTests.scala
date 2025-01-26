package io.github.edadma.chess

class QueenTests extends ChessSpec {
  "inner" - {
    val board = Board.fromString("""
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  q  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |""".stripMargin)

    val attackedSquares = List(
      // Rook-like moves
      (3, 0),
      (3, 1),
      (3, 2),
      (3, 3),
      (3, 4),
      (3, 6),
      (3, 7), // vertical
      (0, 5),
      (1, 5),
      (2, 5),
      (4, 5),
      (5, 5),
      (6, 5),
      (7, 5), // horizontal
      // Bishop-like moves
      (0, 2),
      (1, 3),
      (1, 7),
      (5, 3),
      (6, 2),
      (7, 1), // diagonals
      (5, 7),
    ).map { case (f, r) => r * 8 + f }

    attackedSquares.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "center square should not be attacked" in {
      assert(!board.isSquareAttacked(3 * 8 + 2, Black))
    }

    "should be 25 squares" in {
      board.getMoves(Black).length shouldBe 25
    }
  }

  "blocked by friendly piece" in {
    val board = Board.fromString("""
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  q  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  p  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |""".stripMargin)

    assert(!board.isSquareAttacked(fromAlgebraic("e4"), Black))
    board.getMoves(Black).length shouldBe 25
  }

  "corner" - {
    val board = Board.fromString("""
                                   |.  .  .  .  .  .  .  q
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |.  .  .  .  .  .  .  .
                                   |""".stripMargin)

    val expectedAttacks = List(
      (0, 7),
      (1, 7),
      (2, 7),
      (3, 7),
      (4, 7),
      (5, 7),
      (6, 7), // horizontal
      (7, 0),
      (7, 1),
      (7, 2),
      (7, 3),
      (7, 4),
      (7, 5),
      (7, 6), // vertical
      (6, 6),
      (5, 5),
      (4, 4),
      (3, 3),
      (2, 2),
      (1, 1),
      (0, 0), // diagonal
    ).map { case (f, r) => r * 8 + f }

    expectedAttacks.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "should be 21 squares" in {
      board.getMoves(Black).length shouldBe 21
    }
  }
}
