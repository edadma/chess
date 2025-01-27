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

    board.getMoves(Black).count(_.piece == BlackRook) shouldBe 10
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

  "captures" - {
    "should capture enemy pieces" in {
      val board = Board.fromString(
        """
          |.  .  .  .  .  .  .  .
          |.  .  .  p  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  R  .  .  .  .
          |.  .  .  p  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |""".stripMargin,
      )

      val moves = board.getMoves(White).filter(_.piece == WhiteRook).map(m => (m.fromIndex, m.toIndex)).toSet
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("d7")) // Capture upward
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("d4")) // Capture downward
    }

    "should stop after capture" in {
      val board = Board.fromString(
        """
          |.  .  .  n  .  .  .  .
          |.  .  .  p  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  R  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |""".stripMargin,
      )

      val moves = board.getMoves(White).filter(_.piece == WhiteRook).map(m => (m.fromIndex, m.toIndex)).toSet
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("d7"))
      moves should not contain (fromAlgebraic("d5") -> fromAlgebraic("d8"))
    }
  }

  "blocked by enemy piece" - {
    "horizontal blockage" in {
      val board = Board.fromString(
        """
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  p  R  n  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |""".stripMargin,
      )

      val moves = board.getMoves(White).filter(_.piece == WhiteRook).map(m => (m.fromIndex, m.toIndex)).toSet
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("c5")) // Can capture left
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("e5")) // Can capture right
      moves should not contain (fromAlgebraic("d5") -> fromAlgebraic("b5")) // Cannot move past captured piece
      moves should not contain (fromAlgebraic("d5") -> fromAlgebraic("f5")) // Cannot move past captured piece
    }

    "vertical blockage" in {
      val board = Board.fromString(
        """
          |.  .  .  .  .  .  .  .
          |.  .  .  p  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  R  .  .  .  .
          |.  .  .  n  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |""".stripMargin,
      )

      val moves = board.getMoves(White).filter(_.piece == WhiteRook).map(m => (m.fromIndex, m.toIndex)).toSet
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("d7")) // Can capture up
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("d4")) // Can capture down
      moves should not contain (fromAlgebraic("d5") -> fromAlgebraic("d8")) // Cannot move past captured piece
      moves should not contain (fromAlgebraic("d5") -> fromAlgebraic("d3")) // Cannot move past captured piece
    }
  }

  "mixed capture scenarios" - {
    "should handle multiple potential captures" in {
      val board = Board.fromString(
        """
          |.  .  .  n  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  p  .  .  .  .
          |p  .  p  R  b  .  n  p
          |.  .  .  q  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |""".stripMargin,
      )

      val moves = board.getMoves(White).filter(_.piece == WhiteRook).map(m => (m.fromIndex, m.toIndex)).toSet

      // Horizontal captures
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("c5"))
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("e5"))

      // Vertical captures
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("d6"))
      moves should contain(fromAlgebraic("d5") -> fromAlgebraic("d4"))

      // Should not move past captures
      moves should not contain (fromAlgebraic("d5") -> fromAlgebraic("d8"))
      moves should not contain (fromAlgebraic("d5") -> fromAlgebraic("d3"))
      moves should not contain (fromAlgebraic("d5") -> fromAlgebraic("a5"))
      moves should not contain (fromAlgebraic("d5") -> fromAlgebraic("h5"))

      moves.size shouldBe 4
    }
  }
}
