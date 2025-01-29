package io.github.edadma.chess

class LastMoveToSANTests extends ChessSpec {
  "lastMoveToSAN" - {
    "basic pawn moves" - {
      "single square advance" in {
        val board = Board.fromString("""
                                       |r  n  b  q  k  b  n  r
                                       |p  p  p  p  p  p  p  p
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |P  P  P  P  P  P  P  P
                                       |R  N  B  Q  K  B  N  R
                                       |""".stripMargin)

        val move = Move("e2", "e4", WhitePawn)
        board.applyMove(move).lastMoveToSAN shouldBe "e4"
      }

      "pawn capture" in {
        val board = Board.fromString("""
                                       |r  n  b  q  k  b  n  r
                                       |p  p  p  .  p  p  p  p
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  p  .  .  .  .
                                       |.  .  .  P  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |P  P  P  .  P  P  P  P
                                       |R  N  B  Q  K  B  N  R
                                       |""".stripMargin)

        val move = Move("d4", "e5", WhitePawn)
        board.applyMove(move).lastMoveToSAN shouldBe "dxe5"
      }
    }

    "piece moves" - {
      "knight move" in {
        val board = Board.fromString("""
                                       |r  n  b  q  k  b  n  r
                                       |p  p  p  p  p  p  p  p
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |P  P  P  P  P  P  P  P
                                       |R  N  B  Q  K  B  N  R
                                       |""".stripMargin)

        val move = Move("b1", "c3", WhiteKnight)
        board.applyMove(move).lastMoveToSAN shouldBe "Nc3"
      }

      "bishop capture" in {
        val board = Board.fromString("""
                                       |r  n  b  q  k  b  n  r
                                       |p  p  p  p  .  p  p  p
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  p  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  B  .  .  .  .  .
                                       |P  P  P  P  P  P  P  P
                                       |R  N  .  Q  K  B  N  R
                                       |""".stripMargin)

        val move = Move("c3", "e5", WhiteBishop)
        board.applyMove(move).lastMoveToSAN shouldBe "Bxe5"
      }
    }

    "castling" - {
      "kingside castle" in {
        val board = Board.fromString("""
                                       |r  n  b  q  k  .  .  r
                                       |p  p  p  p  p  p  p  p
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |P  P  P  P  P  P  P  P
                                       |R  N  B  Q  K  .  .  R
                                       |""".stripMargin)

        val move = Move("e1", "g1", WhiteKing, MoveType.CASTLE_KINGSIDE)
        board.applyMove(move).lastMoveToSAN shouldBe "O-O"
      }

      "queenside castle" in {
        val board = Board.fromString("""
                                       |r  .  .  .  k  b  n  r
                                       |p  p  p  p  p  p  p  p
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |P  P  P  P  P  P  P  P
                                       |R  .  .  .  K  B  N  R
                                       |""".stripMargin)

        val move = Move("e1", "c1", WhiteKing, MoveType.CASTLE_QUEENSIDE)
        board.applyMove(move).lastMoveToSAN shouldBe "O-O-O"
      }
    }

    "check and checkmate" - {
      "move giving check" in {
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

        val move = Move("e4", "e7", WhiteQueen)
        board.applyMove(move).lastMoveToSAN shouldBe "Qe7+"
      }

      "checkmate" in {
        val board = Board.fromString("""
                                       |.  .  .  .  k  .  .  r
                                       |.  .  .  .  .  .  R  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  K  .  .  .
                                       |""".stripMargin)

        val move = Move("g7", "e7", WhiteRook)
        board.applyMove(move).lastMoveToSAN shouldBe "Re7#"
      }
    }

    "pawn promotion" - {
      "simple promotion" in {
        val board = Board.fromString("""
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  P  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  k  .  .  .
                                       |""".stripMargin)

        val move = Move("d7", "d8", WhitePawn, MoveType.NORMAL, Some(WhiteQueen))
        board.applyMove(move).lastMoveToSAN shouldBe "d8=Q"
      }

      "promotion with capture" in {
        val board = Board.fromString("""
                                       |.  .  .  n  .  .  .  .
                                       |.  .  .  P  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  k  .  .  .
                                       |""".stripMargin)

        val move = Move("d7", "e8", WhitePawn, MoveType.NORMAL, Some(WhiteQueen))
        board.applyMove(move).lastMoveToSAN shouldBe "dxe8=Q"
      }
    }

    "disambiguation" - {
      "when two rooks can move to same square" in {
        val board = Board.fromString("""
                                       |.  .  .  .  k  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  R  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  R  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  K  .  .  .
                                       |""".stripMargin)

        val move = Move("b5", "b4", WhiteRook)
        board.applyMove(move).lastMoveToSAN shouldBe "R5b4"
      }

      "when two knights can move to same square" in {
        val board = Board.fromString("""
                                       |.  .  .  .  k  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  N  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  N  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  .  .  .  .
                                       |.  .  .  .  K  .  .  .
                                       |""".stripMargin)

        val move = Move("c6", "e5", WhiteKnight)
        board.applyMove(move).lastMoveToSAN shouldBe "N6e5"
      }
    }
  }
}
