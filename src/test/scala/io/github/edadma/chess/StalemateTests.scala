package io.github.edadma.chess

class StalemateTests extends ChessSpec {
  "isStalemate" - {
    "basic stalemate position" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  k  .
                                     |.  .  .  .  .  .  .  p
                                     |.  .  .  .  .  .  .  K
                                     |""".stripMargin)

      board.isStalemate(Black) shouldBe false
      board.isStalemate(White) shouldBe true
    }

    "not stalemate when moves exist" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  k  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  K  .  .  .
                                     |""".stripMargin)

      board.isStalemate(Black) shouldBe false
    }

    "not stalemate when in check" in {
      val board = Board.fromString("""
                                     |.  .  .  .  k  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  Q  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  K  .  .  .
                                     |""".stripMargin)

      board.isStalemate(Black) shouldBe false
    }

    "lone king stalemate" in {
      val board = Board.fromString("""
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  R  .  R  .  .
                                     |.  .  .  .  k  .  .  .
                                     |.  .  .  R  .  R  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  K  .  .  .
                                     |""".stripMargin)

      board.isStalemate(Black) shouldBe true
    }
  }
}
