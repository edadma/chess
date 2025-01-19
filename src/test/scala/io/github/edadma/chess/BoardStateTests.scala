package io.github.edadma.chess

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BoardStateTests extends AnyFreeSpec with Matchers {
  "Initial Position" - {
    "should have correct starting pieces" in {
      val board = Board()

      // White pieces
      board.getPiece(0) shouldBe Some(WhiteRook)
      board.getPiece(1) shouldBe Some(WhiteKnight)
      board.getPiece(2) shouldBe Some(WhiteBishop)
      board.getPiece(3) shouldBe Some(WhiteQueen)
      board.getPiece(4) shouldBe Some(WhiteKing)
      board.getPiece(5) shouldBe Some(WhiteBishop)
      board.getPiece(6) shouldBe Some(WhiteKnight)
      board.getPiece(7) shouldBe Some(WhiteRook)

      // White pawns
      (8 to 15).foreach(sq => board.getPiece(sq) shouldBe Some(WhitePawn))

      // Empty squares
      (16 to 47).foreach(sq => board.getPiece(sq) shouldBe None)

      // Black pawns
      (48 to 55).foreach(sq => board.getPiece(sq) shouldBe Some(BlackPawn))

      // Black pieces
      board.getPiece(56) shouldBe Some(BlackRook)
      board.getPiece(57) shouldBe Some(BlackKnight)
      board.getPiece(58) shouldBe Some(BlackBishop)
      board.getPiece(59) shouldBe Some(BlackQueen)
      board.getPiece(60) shouldBe Some(BlackKing)
      board.getPiece(61) shouldBe Some(BlackBishop)
      board.getPiece(62) shouldBe Some(BlackKnight)
      board.getPiece(63) shouldBe Some(BlackRook)
    }
  }

  "Custom Position" - {
    "should load correctly from string representation" in {
      val layout = """
        |r  .  b  .  k  b  .  r
        |p  p  p  p  p  p  p  p
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |.  .  .  .  .  .  .  .
        |P  P  P  P  P  P  P  P
        |R  .  B  .  K  B  .  R
        """.stripMargin

      val board = Board.fromString(layout)

      board.getPiece(0) shouldBe Some(WhiteRook)
      board.getPiece(2) shouldBe Some(WhiteBishop)
      board.getPiece(4) shouldBe Some(WhiteKing)

      board.getPiece(56) shouldBe Some(BlackRook)
      board.getPiece(58) shouldBe Some(BlackBishop)
      board.getPiece(60) shouldBe Some(BlackKing)
    }
  }
}
