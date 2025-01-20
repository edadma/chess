package io.github.edadma.chess

import Board.*

import pprint.pprintln

class PieceMovementEdgeCaseTests extends ChessSpec {
  "Pinned piece movement" - {
    "not allow pinned bishop to move" in {
      val board = Board.fromString(
        """
          |.  .  .  .  r  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  B  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  K  .  .  .
          |.  .  .  .  .  .  .  .
          |.  .  .  .  .  .  .  .""".stripMargin.trim,
      )

      // Bishop is pinned to king by rook, should have no legal moves
      board.generateLegalMoves(White).filter(_.piece == WhiteBishop) shouldBe empty
    }
  }

}
