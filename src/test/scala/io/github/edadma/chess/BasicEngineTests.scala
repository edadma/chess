package io.github.edadma.chess

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BasicEngineTests extends AnyFreeSpec with Matchers {
  "BasicEngine" - {
    "when playing opening moves" - {
      "should make valid first moves" in {
        val game = new Game
        game.initialize()
        val engine = new BasicEngine

        val move = engine.makeMove(game)
        move shouldBe defined
        game.makeMove(move.get) shouldBe true
      }

      "should develop pieces early" in {
        val game = new Game
        game.initialize()
        val engine              = new BasicEngine
        var knightOrBishopMoved = false

        // Play first 5 moves
        for (_ <- 1 to 5) {
          val move = engine.makeMove(game).get
          if (
            game.getPiece(move.from).get.pieceType == Knight ||
            game.getPiece(move.from).get.pieceType == Bishop
          ) {
            knightOrBishopMoved = true
          }
          game.makeMove(move)
        }

        knightOrBishopMoved shouldBe true
      }
    }

    "when evaluating captures" - {
      "should capture undefended pieces" in {
        val game = new Game
        game.initialize()

        // Clear the board first
        val squares = for {
          file <- 'a' to 'h'
          rank <- 1 to 8
        } yield Square(file, rank)
        squares.foreach(game.removePiece)

        // Set up only the pieces we want
        game.placePiece(Square('e', 4), Piece(Pawn, White))
        game.placePiece(Square('d', 5), Piece(Pawn, Black))

        val engine = new BasicEngine
        val move   = engine.makeMove(game)

        move.get.to shouldBe Square('d', 5)
      }

      "should prefer capturing higher value pieces" in {
        val game = new Game
        game.initialize()

        // Expose both a pawn and a queen
        game.makeMove(Move(Square('e', 2), Square('e', 4)))
        game.makeMove(Move(Square('d', 7), Square('d', 5)))
        game.makeMove(Move(Square('e', 4), Square('e', 5)))
        game.makeMove(Move(Square('d', 8), Square('d', 6)))

        val engine = new BasicEngine
        val move   = engine.makeMove(game)

        // Should capture queen instead of pawn
        move.get.to shouldBe Square('d', 6)
      }
    }

    "when detecting checkmate opportunities" - {
      "should execute fool's mate when available" in {
        val game = new Game
        game.initialize()

        game.makeMove(Move(Square('f', 2), Square('f', 3)))
        game.makeMove(Move(Square('e', 7), Square('e', 5)))
        game.makeMove(Move(Square('g', 2), Square('g', 4)))

        val engine = new BasicEngine
        val move   = engine.makeMove(game)

        move shouldBe Some(Move(Square('d', 8), Square('h', 4)))
        game.makeMove(move.get)
        game.isCheckmate(White) shouldBe true
      }

      "should prefer checkmate over material gain" in {
        val game = new Game

        // Fool's mate position with queen to capture
        game.placePiece(Square('g', 1), Piece(King, White))
        game.placePiece(Square('f', 2), Piece(Pawn, White))
        game.placePiece(Square('g', 2), Piece(Pawn, White))
        game.placePiece(Square('d', 8), Piece(Queen, Black))
        game.placePiece(Square('f', 3), Piece(Queen, White)) // Tempting capture

        val engine = new BasicEngine
        val move   = engine.makeMove(game)

        move shouldBe Some(Move(Square('d', 8), Square('h', 4)))
        game.makeMove(move.get)
        game.isCheckmate(White) shouldBe true
      }
    }

    "when in check" - {
      "should escape check when possible" in {
        val game = new Game
        game.initialize()

        // Create a check situation
        game.makeMove(Move(Square('e', 2), Square('e', 4)))
        game.makeMove(Move(Square('f', 7), Square('f', 6)))
        game.makeMove(Move(Square('d', 2), Square('d', 4)))
        game.makeMove(Move(Square('d', 8), Square('h', 4)))

        val engine = new BasicEngine
        val move   = engine.makeMove(game)

        move shouldBe defined
        val tempGame = new Game
        tempGame.copyFrom(game)
        tempGame.makeMove(move.get)
        tempGame.isCheck(White) shouldBe false
      }
    }
  }
}
