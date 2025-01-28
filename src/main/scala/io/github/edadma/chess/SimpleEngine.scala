package io.github.edadma.chess

class SimpleEngine extends Engine {
  // Basic piece values
  private val pieceValues = Map[PieceType, Int](
    PieceType.PAWN   -> 100,
    PieceType.KNIGHT -> 320,
    PieceType.BISHOP -> 330,
    PieceType.ROOK   -> 500,
    PieceType.QUEEN  -> 900,
    PieceType.KING   -> 20000,
  )

  // Piece-square tables (simplified) - encourage good piece positioning
  private val pawnTable = Array(
    0, 0, 0, 0, 0, 0, 0, 0,
    50, 50, 50, 50, 50, 50, 50, 50,
    10, 10, 20, 30, 30, 20, 10, 10,
    5, 5, 10, 25, 25, 10, 5, 5,
    0, 0, 0, 20, 20, 0, 0, 0,
    5, -5, -10, 0, 0, -10, -5, 5,
    5, 10, 10, -20, -20, 10, 10, 5,
    0, 0, 0, 0, 0, 0, 0, 0,
  )

  private val knightTable = Array(
    -50, -40, -30, -30, -30, -30, -40, -50,
    -40, -20, 0, 0, 0, 0, -20, -40,
    -30, 0, 10, 15, 15, 10, 0, -30,
    -30, 5, 15, 20, 20, 15, 5, -30,
    -30, 0, 15, 20, 20, 15, 0, -30,
    -30, 5, 10, 15, 15, 10, 5, -30,
    -40, -20, 0, 5, 5, 0, -20, -40,
    -50, -40, -30, -30, -30, -30, -40, -50,
  )

  def makeMove(game: Game): Option[ChessMove] = {
    val moves = game.getBoard.getMoves(game.getCurrentTurn).toList
    if (moves.isEmpty) return None

    // For each move, evaluate the resulting position
    val moveScores = moves.map { move =>
      val newBoard = game.getBoard.applyMove(move)
      val score    = evaluatePosition(newBoard, game.getCurrentTurn)
      (move, score)
    }

    // Check for checkmate moves first
    val checkmatingMoves = moves.filter(move => {
      val newBoard = game.getBoard.applyMove(move)
      newBoard.isCheckmate(game.getCurrentTurn.opposite)
    })

    if (checkmatingMoves.nonEmpty) {
      Some(checkmatingMoves.head) // Found a checkmate - take it!
    } else {
      // Otherwise select the move with the highest score
      Some(moveScores.maxBy(_._2)._1)
    }
  }

  private def evaluatePosition(board: ChessBoard, side: Side): Int = {
    // First check for checkmate
    if (board.isCheckmate(side.opposite)) {
      return Int.MaxValue // This is a winning position
    }
    if (board.isCheckmate(side)) {
      return Int.MinValue // This is a losing position
    }

    var score = 0

    // Add bonus for checking the opponent's king
    if (board.isInCheck(side.opposite)) {
      score += 50 // Encourage checks as they might lead to checkmate
    }

    // Material and piece position evaluation
    board.getPieces.foreach { case (square, piece) =>
      val pieceValue    = pieceValues(piece.pieceType)
      val positionValue = getPiecePositionValue(piece, square)

      val value = pieceValue + positionValue
      score += (if (piece.side == side) value else -value)
    }

    // Mobility (piece movement options)
    val mobilityScore = board.getMoves(side).size * 10
    score += mobilityScore

    // Pawn structure
    score += evaluatePawnStructure(board, side)

    // Control of center
    score += evaluateCenterControl(board, side)

    // King safety
    score += evaluateKingSafety(board, side)

    score
  }

  private def getPiecePositionValue(piece: Piece, square: Int): Int = {
    val table = piece.pieceType match {
      case PieceType.PAWN   => pawnTable
      case PieceType.KNIGHT => knightTable
      case _                => return 0 // Simplified - only using pawn and knight tables for example
    }

    val index = if (piece.side == White) square else 63 - square
    table(index)
  }

  private def evaluatePawnStructure(board: ChessBoard, side: Side): Int = {
    var score = 0

    // Detect doubled pawns (pawns on same file)
    val pawnFiles = board.getPiecesBySide(side)
      .filter(_._2.pieceType == PieceType.PAWN)
      .map(square => square._1 % 8)
      .toList

    val doubledPawns = pawnFiles.groupBy(identity).count(_._2.size > 1)
    score -= doubledPawns * 20

    // Detect passed pawns (no enemy pawns ahead)
    board.getPiecesBySide(side).foreach { case (square, piece) =>
      if (piece.pieceType == PieceType.PAWN) {
        val file = square % 8
        val rank = square / 8
        val isPassed = !board.getPiecesBySide(side.opposite)
          .exists { case (sq, p) =>
            p.pieceType == PieceType.PAWN &&
            sq % 8 == file &&
            (if (side == White) sq / 8 > rank else sq / 8 < rank)
          }
        if (isPassed) score += 30
      }
    }

    score
  }

  private def evaluateCenterControl(board: ChessBoard, side: Side): Int = {
    val centerSquares = List(27, 28, 35, 36) // e4, d4, e5, d5
    val score         = centerSquares.count(square => board.isSquareAttacked(square, side))
    score * 10
  }

  private def evaluateKingSafety(board: ChessBoard, side: Side): Int = {
    var score = 0

    // King piece-square table for endgame
    val kingEndgameTable = Array(
      -30, -40, -40, -50, -50, -40, -40, -30,
      -30, -40, -40, -50, -50, -40, -40, -30,
      -30, -40, -40, -50, -50, -40, -40, -30,
      -30, -40, -40, -50, -50, -40, -40, -30,
      -20, -30, -30, -40, -40, -30, -30, -20,
      -10, -20, -20, -20, -20, -20, -20, -10,
      20, 20, 0, 0, 0, 0, 20, 20,
      20, 30, 10, 0, 0, 10, 30, 20,
    )

    // Find the king
    board.getPiecesByType(Set(PieceType.KING), side).foreach { kingSquare =>
      // Penalize exposed king
      val attackedSquaresAroundKing = List(
        kingSquare - 9,
        kingSquare - 8,
        kingSquare - 7,
        kingSquare - 1,
        kingSquare + 1,
        kingSquare + 7,
        kingSquare + 8,
        kingSquare + 9,
      ).count(square =>
        square >= 0 && square < 64 &&
          board.isSquareAttacked(square, side.opposite),
      )

      score -= attackedSquaresAroundKing * 10

      // Use endgame king positioning
      val index = if (side == White) kingSquare else 63 - kingSquare
      score += kingEndgameTable(index)
    }

    score
  }
}
