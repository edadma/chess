package io.github.edadma.chess

class NestedEngine extends Engine {
  private val MaxDepth = 3
  private val pieceValues = Map(
    Pawn   -> 100,
    Knight -> 320,
    Bishop -> 330,
    Rook   -> 500,
    Queen  -> 900,
    King   -> 20000,
  )

  def makeMove(game: Game): Option[Move] = {
    val moves = game.getAllLegalMoves
    if (moves.isEmpty) return None

    moves.maxByOption { move =>
      val tempGame = new Game
      tempGame.copyFrom(game)
      tempGame.makeMove(move)
      -minimax(tempGame, MaxDepth - 1, Int.MinValue, Int.MaxValue, maximizing = false)
    }
  }

  private def minimax(game: Game, depth: Int, alpha: Int, beta: Int, maximizing: Boolean): Int = {
    if (depth == 0) return evaluatePosition(game)

    val moves = game.getAllLegalMoves
    if (moves.isEmpty) return evaluatePosition(game)

    if (maximizing) {
      var value = Int.MinValue
      var a     = alpha
      for (move <- moves if value < beta) {
        val tempGame = new Game
        tempGame.copyFrom(game)
        tempGame.makeMove(move)
        value = math.max(value, minimax(tempGame, depth - 1, a, beta, false))
        a = math.max(a, value)
      }
      value
    } else {
      var value = Int.MaxValue
      var b     = beta
      for (move <- moves if value > alpha) {
        val tempGame = new Game
        tempGame.copyFrom(game)
        tempGame.makeMove(move)
        value = math.min(value, minimax(tempGame, depth - 1, alpha, b, true))
        b = math.min(b, value)
      }
      value
    }
  }

  private def evaluatePosition(game: Game): Int = {
    var score = 0

    // Material evaluation using squares a1 to h8
    for {
      file <- 'a' to 'h'
      rank <- 1 to 8
      square = Square(file, rank)
      piece <- game.getPiece(square)
    } score += (if (piece.color == game.getCurrentTurn) pieceValues(piece.pieceType) else -pieceValues(piece.pieceType))

    // Center control
    val centerSquares = List(
      Square('e', 4),
      Square('d', 4),
      Square('e', 5),
      Square('d', 5),
    )

    for {
      square <- centerSquares
      piece  <- game.getPiece(square)
    } score += (if (piece.color == game.getCurrentTurn) 30 else -30)

    // Mobility (legally accessible squares)
    val currentMoves = game.getAllLegalMoves.size
    score + currentMoves * 10
  }
}
