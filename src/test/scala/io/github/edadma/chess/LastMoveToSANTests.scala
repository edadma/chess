package io.github.edadma.chess

class LastMoveToSANTests extends ChessSpec {
  "lastMoveToSAN" - {
    "basic pawn moves" - {
      "single square advance" in {
        val game = new Game
        game.makeMove(Move("e2", "e4", WhitePawn))
        game.lastMoveToSAN shouldBe "e4"
      }

      "pawn capture" in {
        val game = new Game(Board.fromString("""
                                               |r  n  b  q  k  b  n  r
                                               |p  p  p  p  .  p  p  p
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  p  .  .  .
                                               |.  .  .  P  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |P  P  P  .  P  P  P  P
                                               |R  N  B  Q  K  B  N  R
                                               |""".stripMargin))
        game.makeMove(Move("d4", "e5", WhitePawn))
        game.lastMoveToSAN shouldBe "dxe5"
      }
    }

    "piece moves" - {
      "knight move" in {
        val game = new Game
        game.makeMove(Move("b1", "c3", WhiteKnight))
        game.lastMoveToSAN shouldBe "Nc3"
      }

      "bishop capture" in {
        val game = new Game(Board.fromString("""
                                               |r  n  b  q  k  b  n  r
                                               |p  p  p  p  .  p  p  p
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  p  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  B  .  .  .  .  .
                                               |P  P  P  P  P  P  P  P
                                               |R  N  .  Q  K  B  N  R
                                               |""".stripMargin))
        game.makeMove(Move("c3", "e5", WhiteBishop))
        game.lastMoveToSAN shouldBe "Bxe5"
      }
    }

    "castling" - {
      "kingside castle" in {
        val game = new Game(Board.fromString("""
                                               |r  n  b  q  k  .  .  r
                                               |p  p  p  p  p  p  p  p
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |P  P  P  P  P  P  P  P
                                               |R  N  B  Q  K  .  .  R
                                               |""".stripMargin))
        game.makeMove(Move("e1", "g1", WhiteKing, MoveType.CASTLE_KINGSIDE))
        game.lastMoveToSAN shouldBe "O-O"
      }

      "queenside castle" in {
        val game = new Game(Board.fromString("""
                                               |r  .  .  .  k  b  n  r
                                               |p  p  p  p  p  p  p  p
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |P  P  P  P  P  P  P  P
                                               |R  .  .  .  K  B  N  R
                                               |""".stripMargin))
        game.makeMove(Move("e1", "c1", WhiteKing, MoveType.CASTLE_QUEENSIDE))
        game.lastMoveToSAN shouldBe "O-O-O"
      }
    }

    "check and checkmate" - {
      "move giving check" in {
        val game = new Game(Board.fromString("""
                                               |.  .  .  .  k  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  Q  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  K  .  .  .
                                               |""".stripMargin))
        game.makeMove(Move("e4", "e7", WhiteQueen))
        game.lastMoveToSAN shouldBe "Qe7+"
      }

      "checkmate" in {
        val game = new Game(Board.fromString("""
                                               |.  .  .  .  k  .  .  .
                                               |Q  .  .  .  .  .  R  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  K  .  .  .
                                               |""".stripMargin))
        game.makeMove(Move("g7", "g8", WhiteRook))
        game.lastMoveToSAN shouldBe "Rg8#"
      }
    }

    "disambiguation" - {
      "when two knights can move to same square" in {
        val game = new Game(Board.fromString("""
                                               |.  .  .  .  k  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  N  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  N  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  .  .  .  .
                                               |.  .  .  .  K  .  .  .
                                               |""".stripMargin))
        game.makeMove(Move("c6", "e5", WhiteKnight))
        game.lastMoveToSAN shouldBe "N6e5"
      }
    }
  }
}
