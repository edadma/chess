package io.github.edadma.chess

class PinTests extends ChessSpec {
  "diagonal pin" - {
    "bishop pinned by bishop" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  b  .  .  .  .  .
                                     |.  .  .  B  .  .  .  .
                                     |.  .  .  .  K  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      // Bishop should only be able to move along the pin line
      val moves = board.getMoves(White).filter(_.piece == WhiteBishop).toSet
      println(moves)
      moves.map(_.toIndex) should contain only fromAlgebraic("c6")
    }

    "rook pinned by queen" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  q  R  K  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      val moves = board.getMoves(White).filter(_.piece == WhiteRook).toSet
      moves.map(_.toIndex) should contain only fromAlgebraic("c4")
    }
  }

  "horizontal pin" - {
    "rook pinned by rook" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |r  R  K  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      val moves = board.getMoves(White).filter(_.piece == WhiteRook).toSet
      moves.map(_.toIndex) should contain only fromAlgebraic("a5")
    }

    "knight pinned by queen" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |q  N  .  K  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      // Knight should have no legal moves when pinned
      board.getMoves(White).filter(_.piece == WhiteKnight) shouldBe empty
    }
  }

  "vertical pin" - {
    "rook pinned by queen" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  q  .  .  .  .
                                     |.  .  .  R  .  .  .  .
                                     |.  .  .  K  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      val moves = board.getMoves(White).filter(_.piece == WhiteRook).toSet
      moves.map(_.toIndex) should contain only fromAlgebraic("d7")
    }

    "bishop pinned by rook" in {
      val board = Board.fromString("""
                                     |.  .  .  r  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  B  .  .  .  .
                                     |.  .  .  K  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |""".stripMargin)

      // Bishop should have no legal moves when pinned vertically
      board.getMoves(White).filter(_.piece == WhiteBishop) shouldBe empty
    }
  }
}
