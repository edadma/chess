package io.github.edadma.chess

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class GameTests extends AnyFreeSpec with Matchers {
  "A Game" - {
    "when handling check situations" - {
      "should not allow moves that leave king in check" in {
        val game = new Game

        // Set up a position where a piece is pinned against the king
        // White king on e1, white bishop on e2, black rook on e8
        game.initializeFromString(
          """
          .  .  .  .  r  .  .  .
          .  .  .  .  .  .  .  .
          .  .  .  .  .  .  .  .
          .  .  .  .  .  .  .  .
          .  .  .  .  .  .  .  .
          .  .  .  .  .  .  .  .
          .  .  .  .  B  .  .  .
          .  .  .  .  K  .  .  .
        """,
          White,
        )

        // Try to move the bishop (which should be pinned)
        val illegalMove = Move(Square('e', 2), Square('d', 3))

        // The move should be in getLegalMovesForPiece (basic piece movement)
        game.getLegalMovesForPiece(
          Square('e', 2),
          Piece(Bishop, White),
        ).map(_.toAlgebraic) should contain(illegalMove.toAlgebraic)

        // But shouldn't be in getAllLegalMoves (considering check)
        game.getAllLegalMoves.map(_.toAlgebraic) should not contain (illegalMove.toAlgebraic)

        // And the move should not be allowed
        game.makeMove(illegalMove) shouldBe false
      }

      "should recognize discovered check" in {
        val game = new Game
        game.initializeFromString(
          """
          .  .  .  .  k  .  .  .
          .  .  .  .  .  .  .  .
          .  .  .  .  B  .  .  .
          .  .  .  .  R  .  .  .
          .  .  .  .  .  .  .  .
          .  .  .  .  .  .  .  .
          .  .  .  .  .  .  .  .
          .  .  .  .  .  .  .  K
        """,
          White,
        )

        // Moving the bishop reveals check from rook
        val move = Move(Square('e', 6), Square('f', 7))
        game.makeMove(move)
        game.isCheck(Black) shouldBe true
      }
    }
  }

  "A Game" - {
    "when newly initialized" - {
      val game = new Game
      game.initialize()

      "should have white pieces in correct starting positions" in {
        game.getPiece(Square('e', 1)) shouldBe Some(Piece(King, White))
        game.getPiece(Square('d', 1)) shouldBe Some(Piece(Queen, White))
        game.getPiece(Square('a', 1)) shouldBe Some(Piece(Rook, White))
        game.getPiece(Square('b', 1)) shouldBe Some(Piece(Knight, White))
        game.getPiece(Square('c', 1)) shouldBe Some(Piece(Bishop, White))
        game.getPiece(Square('f', 1)) shouldBe Some(Piece(Bishop, White))
        game.getPiece(Square('g', 1)) shouldBe Some(Piece(Knight, White))
        game.getPiece(Square('h', 1)) shouldBe Some(Piece(Rook, White))

        // Check all white pawns
        ('a' to 'h').foreach { file =>
          game.getPiece(Square(file, 2)) shouldBe Some(Piece(Pawn, White))
        }
      }

      "should have black pieces in correct starting positions" in {
        game.getPiece(Square('e', 8)) shouldBe Some(Piece(King, Black))
        game.getPiece(Square('d', 8)) shouldBe Some(Piece(Queen, Black))
        game.getPiece(Square('a', 8)) shouldBe Some(Piece(Rook, Black))
        game.getPiece(Square('b', 8)) shouldBe Some(Piece(Knight, Black))
        game.getPiece(Square('c', 8)) shouldBe Some(Piece(Bishop, Black))
        game.getPiece(Square('f', 8)) shouldBe Some(Piece(Bishop, Black))
        game.getPiece(Square('g', 8)) shouldBe Some(Piece(Knight, Black))
        game.getPiece(Square('h', 8)) shouldBe Some(Piece(Rook, Black))

        // Check all black pawns
        ('a' to 'h').foreach { file =>
          game.getPiece(Square(file, 7)) shouldBe Some(Piece(Pawn, Black))
        }
      }

      "should have empty squares in the middle ranks" in {
        for {
          rank <- 3 to 6
          file <- 'a' to 'h'
        } {
          game.getPiece(Square(file, rank)) shouldBe None
        }
      }

      "should start with White's turn" in {
        game.getCurrentTurn shouldBe White
      }
    }

    "when making legal pawn moves" - {
      val game = new Game
      game.initialize()

      "should allow white pawns to move one square forward" in {
        val move = Move(Square('e', 2), Square('e', 3))
        game.makeMove(move) shouldBe true
        game.getPiece(Square('e', 3)) shouldBe Some(Piece(Pawn, White))
        game.getPiece(Square('e', 2)) shouldBe None
      }

      "should allow white pawns to move two squares forward from starting position" in {
        val game = new Game
        game.initialize()
        val move = Move(Square('e', 2), Square('e', 4))
        game.makeMove(move) shouldBe true
        game.getPiece(Square('e', 4)) shouldBe Some(Piece(Pawn, White))
        game.getPiece(Square('e', 2)) shouldBe None
      }
    }

    "when checking move legality" - {
      val game = new Game
      game.initialize()

      "should not allow moving a piece to its own square" in {
        val move = Move(Square('e', 2), Square('e', 2))
        game.makeMove(move) shouldBe false
      }

      "should not allow moving from an empty square" in {
        val move = Move(Square('e', 3), Square('e', 4))
        game.makeMove(move) shouldBe false
      }

      "should not allow white to move black pieces" in {
        val move = Move(Square('e', 7), Square('e', 6))
        game.makeMove(move) shouldBe false
      }

      "should not allow pawn to move diagonally without capture" in {
        val move = Move(Square('e', 2), Square('f', 3))
        game.makeMove(move) shouldBe false
      }
    }

    "when making moves" - {
      "should alternate turns between white and black" in {
        val game = new Game
        game.initialize()

        game.getCurrentTurn shouldBe White
        game.makeMove(Move(Square('e', 2), Square('e', 4))) shouldBe true
        game.getCurrentTurn shouldBe Black
        game.makeMove(Move(Square('e', 7), Square('e', 5))) shouldBe true
        game.getCurrentTurn shouldBe White
      }
    }
  }
}
