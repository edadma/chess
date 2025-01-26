package io.github.edadma.chess

class RookTests extends ChessSpec {
  "center" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  r  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    val attackedSquares = (for {
      i <- 0 to 7
      square <- List(
        (3, i), // horizontal
        (i, 3), // vertical
      )
      if square != (3, 3) // exclude rook's position
    } yield square._1 * 8 + square._2).toList

    attackedSquares.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "should be 14 squares" in {
      board.getMoves(Black).length shouldBe 14
    }
  }

  "blocked by friendly piece" in {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  r  .  .  .  .
        |.  .  .  p  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    assert(!board.isSquareAttacked(fromAlgebraic("d4"), Black)) // Square with black pawn
    assert(!board.isSquareAttacked(fromAlgebraic("d3"), Black)) // Square behind black pawn

    board.getMoves(Black).length shouldBe 10
  }

  "corner" - {
    val board = Board.fromString(
      """
        |.  .  .  .  .  .  .  r
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |""".stripMargin,
    )

    val expectedAttacks = (for {
      i <- 0 to 7
      square <- List(
        (7, i), // horizontal
        (i, 7), // vertical
      )
      if square != (7, 7) // exclude rook's position
    } yield square._1 * 8 + square._2).toList

    expectedAttacks.foreach { square =>
      s"square ${toAlgebraic(square)} should be attacked" in {
        assert(board.isSquareAttacked(square, Black))
      }
    }

    "should be 14 squares" in {
      board.getMoves(Black).length shouldBe 14
    }
  }
}
