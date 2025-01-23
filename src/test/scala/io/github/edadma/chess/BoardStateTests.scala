//package io.github.edadma.chess
//
//import org.scalatest.freespec.AnyFreeSpec
//import org.scalatest.matchers.should.Matchers
//
//class BoardStateTests extends AnyFreeSpec with Matchers {
//  "Initial Position" - {
//    "should have correct starting pieces" in {
//      val board = Board()
//
//      // White pieces
//      board.getPiece(0) shouldBe Some(WhiteRook)
//      board.getPiece(1) shouldBe Some(WhiteKnight)
//      board.getPiece(2) shouldBe Some(WhiteBishop)
//      board.getPiece(3) shouldBe Some(WhiteQueen)
//      board.getPiece(4) shouldBe Some(WhiteKing)
//      board.getPiece(5) shouldBe Some(WhiteBishop)
//      board.getPiece(6) shouldBe Some(WhiteKnight)
//      board.getPiece(7) shouldBe Some(WhiteRook)
//
//      // White pawns
//      (8 to 15).foreach(sq => board.getPiece(sq) shouldBe Some(WhitePawn))
//
//      // Empty squares
//      (16 to 47).foreach(sq => board.getPiece(sq) shouldBe None)
//
//      // Black pawns
//      (48 to 55).foreach(sq => board.getPiece(sq) shouldBe Some(BlackPawn))
//
//      // Black pieces
//      board.getPiece(56) shouldBe Some(BlackRook)
//      board.getPiece(57) shouldBe Some(BlackKnight)
//      board.getPiece(58) shouldBe Some(BlackBishop)
//      board.getPiece(59) shouldBe Some(BlackQueen)
//      board.getPiece(60) shouldBe Some(BlackKing)
//      board.getPiece(61) shouldBe Some(BlackBishop)
//      board.getPiece(62) shouldBe Some(BlackKnight)
//      board.getPiece(63) shouldBe Some(BlackRook)
//    }
//  }
//
//  "Board.fromString" - {
//    "parse initial position" in {
//      val layout =
//        """
//          |r  n  b  q  k  b  n  r
//          |p  p  p  p  p  p  p  p
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |P  P  P  P  P  P  P  P
//          |R  N  B  Q  K  B  N  R
//      """.stripMargin.trim
//
//      val board = Board.fromString(layout)
//      board.blackRooks should be(0x8100000000000000L)
//      board.whitePawns should be(0xff00L)
//    }
//  }
//
//  "Custom Position" - {
//    "should load correctly from string representation" in {
//      val layout = """
//        |r  .  b  .  k  b  .  r
//        |p  p  p  p  p  p  p  p
//        |.  .  .  .  .  .  .  .
//        |.  .  .  .  .  .  .  .
//        |.  .  .  .  .  .  .  .
//        |.  .  .  .  .  .  .  .
//        |P  P  P  P  P  P  P  P
//        |R  .  B  .  K  B  .  R
//        """.stripMargin
//
//      val board = Board.fromString(layout)
//
//      board.getPiece(0) shouldBe Some(WhiteRook)
//      board.getPiece(2) shouldBe Some(WhiteBishop)
//      board.getPiece(4) shouldBe Some(WhiteKing)
//
//      board.getPiece(56) shouldBe Some(BlackRook)
//      board.getPiece(58) shouldBe Some(BlackBishop)
//      board.getPiece(60) shouldBe Some(BlackKing)
//    }
//  }
//
//  "Bitboards" - {
//    "should track occupied squares correctly" in {
//      val board = Board()
//      board.occupied shouldBe board.whitePieces | board.blackPieces
//      board.empty shouldBe ~board.occupied
//
//      // Test specific ranks
//      (board.occupied & 0xffL) shouldBe 0xffL               // First rank full
//      (board.occupied & (0xffL << 8)) shouldBe (0xffL << 8) // Second rank full
//      (board.occupied & (0xffL << 32)) shouldBe 0L          // Middle ranks empty
//    }
//
//    "should track piece bitboards correctly" in {
//      val board = Board()
//
//      board.whitePawns shouldBe 0xff00L
//      board.whiteKnights shouldBe 0x42L
//      board.whiteBishops shouldBe 0x24L
//      board.whiteRooks shouldBe 0x81L
//      board.whiteQueens shouldBe 0x8L
//      board.whiteKing shouldBe 0x10L
//
//      board.blackPawns shouldBe 0xff000000000000L
//      board.blackKnights shouldBe 0x4200000000000000L
//      board.blackBishops shouldBe 0x2400000000000000L
//      board.blackRooks shouldBe 0x8100000000000000L
//      board.blackQueens shouldBe 0x800000000000000L
//      board.blackKing shouldBe 0x1000000000000000L
//    }
//  }
//
//  "Board validation" - {
//    "reject boards with wrong number of rows" in {
//      val badLayout =
//        """
//          |r  n  b  q  k  b  n  r
//          |p  p  p  p  p  p  p  p
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |P  P  P  P  P  P  P  P
//          |R  N  B  Q  K  B  N  R
//     """.stripMargin.trim
//
//      intercept[IllegalArgumentException] {
//        Board.fromString(badLayout)
//      }
//    }
//
//    "reject invalid piece characters" in {
//      val badPiece =
//        """
//          |r  n  b  q  k  b  n  r
//          |p  p  p  p  p  p  p  p
//          |.  .  .  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |.  .  x  .  .  .  .  .
//          |.  .  .  .  .  .  .  .
//          |P  P  P  P  P  P  P  P
//          |R  N  B  Q  K  B  N  R
//     """.stripMargin.trim
//
//      intercept[IllegalArgumentException] {
//        Board.fromString(badPiece)
//      }
//    }
//  }
//}
