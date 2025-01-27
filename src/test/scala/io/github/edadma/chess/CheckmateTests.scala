package io.github.edadma.chess

class CheckmateTests extends ChessSpec {
  "isCheckmate" - {
    "fool's mate" in {
      val board = Board.fromString("""
                                     |r  n  b  .  k  b  n  r
                                     |p  p  p  p  .  p  p  p
                                     |.  .  .  .  p  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  P  q
                                     |.  .  .  .  .  P  .  .
                                     |P  P  P  P  P  .  .  P
                                     |R  N  B  Q  K  B  N  R
                                     |""".stripMargin)

      board.isCheckmate(White) shouldBe true
    }

    "not checkmate when king can escape" in {
      val board = Board.fromString("""
                                     |.  .  .  .  k  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  Q  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  K  .  .  .
                                     |""".stripMargin)

      board.isCheckmate(Black) shouldBe false
    }

    "not checkmate when piece can block" in {
      val board = Board.fromString("""
                                     |.  .  .  .  k  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  r  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  Q  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  K  .  .  .
                                     |""".stripMargin)

      board.isCheckmate(Black) shouldBe false
    }

    "not checkmate when piece can capture attacker" in {
      val board = Board.fromString("""
                                     |.  .  .  .  k  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  r  .  .  .
                                     |.  .  .  .  Q  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  .  .  .  .
                                     |.  .  .  .  K  .  .  .
                                     |""".stripMargin)

      board.isCheckmate(Black) shouldBe false
    }
  }
}
