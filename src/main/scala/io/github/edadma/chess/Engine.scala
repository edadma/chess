package io.github.edadma.chess

// Trait defining the interface for all chess engines
trait Engine {
  def makeMove(game: Game): Option[Move]
}

// Basic implementation of a chess engine
class BasicEngine extends Engine {
  // Basic piece values used for move evaluation
  private val pieceValues: Map[PieceType, Int] = Map(
    Pawn   -> 1000, // Increased to make captures more attractive
    Knight -> 3200,
    Bishop -> 3300,
    Rook   -> 5000,
    Queen  -> 9000,
    King   -> 200000,
  )

  // Basic position evaluation for pieces (center control is better)
  private def getPositionValue(square: Square): Int = {
    val centerValue = 4 - (Math.abs('e' - square.file) + Math.abs(4.5 - square.rank))
    centerValue.toInt * 10
  }

  def makeMove(game: Game): Option[Move] = {
    // Get all legal moves for the current player
    val legalMoves = game.getAllLegalMoves

    if (legalMoves.isEmpty) return None

    // Evaluate each move and select the best one
    val evaluatedMoves = legalMoves.map { move =>
      val score = evaluateMove(game, move)
      (move, score)
    }

    // Select the move with the highest score
    Some(evaluatedMoves.maxBy(_._2)._1)
  }

  // Evaluate a move based on material gain/loss and position improvement
  private def evaluateMove(game: Game, move: Move): Int = {
    var score = 0

    // Get the moving piece
    val movingPiece = game.getPiece(move.from).get

    // Make a temporary copy of the game to simulate the move
    val tempGame = new Game
    tempGame.copyFrom(game)
    tempGame.makeMove(move)

    // Check if this move gives checkmate
    if (tempGame.isCheckmate(game.getCurrentTurn.opposite)) {
      return Int.MaxValue // Highest priority - always choose checkmate
    }

    // Check if this move gives check
    if (tempGame.isCheck(game.getCurrentTurn.opposite)) {
      score += 500 // Bonus for check
    }

    // Calculate material gain/loss
    val capturedPiece = game.getPiece(move.to)
    capturedPiece.foreach { piece =>
      score += pieceValues(piece.pieceType)
    }

    // Add position value for the destination square
    score += getPositionValue(move.to)

    // Consider developing pieces in early game (bonus for knights and bishops)
    if (movingPiece.pieceType == Knight || movingPiece.pieceType == Bishop) {
      val isStartingRank = movingPiece.color match {
        case White => move.from.rank == 1
        case Black => move.from.rank == 8
      }
      if (isStartingRank) {
        score += 50 // Bonus for developing pieces
      }
    }

    // Add some randomness to prevent repetitive moves
    score += scala.util.Random.nextInt(10)

    score
  }
}

// Factory object to create different types of engines
object Engine {
  def createBasicEngine(): Engine = new BasicEngine()
}
