package io.github.edadma.chess

class SimpleEngine extends Engine {
  // Basic piece values
  private val pieceValues = Map[PieceType, Int](
    PieceType.PAWN   -> 100,
    PieceType.KNIGHT -> 300,
    PieceType.BISHOP -> 300,
    PieceType.ROOK   -> 500,
    PieceType.QUEEN  -> 900,
    PieceType.KING   -> 10000,
  )

  def makeMove(game: Game): Option[ChessMove] = {
    val moves = game.getBoard.getMoves(game.getCurrentTurn).toList
    if (moves.isEmpty) return None

    // First, check for immediate checkmate
    val mateMoves = moves.filter(move => {
      val newBoard = game.getBoard.applyMove(move)
      newBoard.isCheckmate(game.getCurrentTurn.opposite)
    })

    if (mateMoves.nonEmpty) return Some(mateMoves.head)

    // Evaluate each move
    val scoredMoves = moves.map(move => {
      val score = evaluateMove(game.getBoard, move, game.getCurrentTurn)
      (move, score)
    })

    // Choose the move with the highest score
    Some(scoredMoves.maxBy(_._2)._1)
  }

  private def evaluateMove(board: ChessBoard, move: ChessMove, side: Side): Int = {
    val newBoard = board.applyMove(move)

    // Start with material evaluation
    var score = evaluateMaterial(board, newBoard, move, side)

    // If move loses material, apply heavy penalty
    if (score < -50) { // Allow slight material loss if position is good
      return score * 10
    }

    // Add positional evaluation
    score += evaluatePosition(move.toIndex)

    // Evaluate piece safety
    score += evaluateSafety(newBoard, move, side)

    // Evaluate check only if it doesn't lose material
    if (newBoard.isInCheck(side.opposite)) {
      score += evaluateCheck(newBoard, move, side)
    }

    score
  }

  private def evaluateMaterial(board: ChessBoard, newBoard: ChessBoard, move: ChessMove, side: Side): Int = {
    // Calculate material change
    val oldMaterial = countMaterial(board, side) - countMaterial(board, side.opposite)
    val newMaterial = countMaterial(newBoard, side) - countMaterial(newBoard, side.opposite)

    (newMaterial - oldMaterial) * 100 // Material is primary concern
  }

  private def countMaterial(board: ChessBoard, side: Side): Int = {
    board.getPiecesBySide(side).map { case (_, piece) =>
      pieceValues(piece.pieceType)
    }.sum
  }

  private def evaluateSafety(board: ChessBoard, move: ChessMove, side: Side): Int = {
    var score = 0

    // Penalty for moving to attacked square
    if (board.isSquareAttacked(move.toIndex, side.opposite)) {
      val movedPieceValue = pieceValues(move.piece.pieceType)
      score -= movedPieceValue // Basic safety penalty
    }

    score
  }

  private def evaluateCheck(board: ChessBoard, move: ChessMove, side: Side): Int = {
    var score = 0

    // Give small bonus for check only if:
    // 1. Our piece giving check is not under attack
    // 2. The opponent's king has limited escape squares
    if (!board.isSquareAttacked(move.toIndex, side.opposite)) {
      val escapeMoves = board.getMoves(side.opposite).count { m =>
        m.piece.pieceType == PieceType.KING
      }

      // Only value check if king has few escape squares
      if (escapeMoves <= 3) {
        score += 50 // Small bonus for threatening check
      }
    }

    score
  }

  private def evaluatePosition(square: Int): Int = {
    val file = square % 8
    val rank = square / 8

    // Simple center control bonus
    val centerFile = 3 - math.abs(file - 3.5).toInt
    val centerRank = 3 - math.abs(rank - 3.5).toInt

    (centerFile + centerRank) * 10
  }
}

//package io.github.edadma.chess
//
//class SimpleEngine extends Engine {
//  // Basic piece values
//  private val pieceValues = Map[PieceType, Int](
//    PieceType.PAWN   -> 100,
//    PieceType.KNIGHT -> 300,
//    PieceType.BISHOP -> 300,
//    PieceType.ROOK   -> 500,
//    PieceType.QUEEN  -> 900,
//    PieceType.KING   -> 10000,
//  )
//
//  def makeMove(game: Game): Option[ChessMove] = {
//    val moves = game.getBoard.getMoves(game.getCurrentTurn).toList
//    if (moves.isEmpty) return None
//
//    // First, check for immediate checkmate
//    val mateMoves = moves.filter(move => {
//      val newBoard = game.getBoard.applyMove(move)
//      newBoard.isCheckmate(game.getCurrentTurn.opposite)
//    })
//
//    if (mateMoves.nonEmpty) return Some(mateMoves.head)
//
//    // If a piece is attacked, prioritize saving it
//    val currentBoard = game.getBoard
//    val currentSide  = game.getCurrentTurn
//
//    // Find attacked pieces, prioritize saving valuable pieces
//    val attackedPieces = currentBoard.getPiecesBySide(currentSide).filter { case (square, piece) =>
//      currentBoard.isSquareAttacked(square, currentSide.opposite)
//    }.toList.sortBy { case (_, piece) => -pieceValues(piece.pieceType) } // Sort by value, highest first
//
//    // If we have attacked pieces, focus on saving them
//    if (attackedPieces.nonEmpty) {
//      val savingMoves = moves.filter { move =>
//        attackedPieces.exists { case (square, _) => move.fromIndex == square }
//      }
//
//      if (savingMoves.nonEmpty) {
//        // Evaluate saving moves with extra weight on safety
//        val scoredSavingMoves = savingMoves.map(move => {
//          val score = evaluateMove(currentBoard, move, currentSide) * 2 // Double weight for saving pieces
//          (move, score)
//        })
//        return Some(scoredSavingMoves.maxBy(_._2)._1)
//      }
//    }
//
//    // Normal evaluation for other moves
//    val scoredMoves = moves.map(move => {
//      val score = evaluateMove(currentBoard, move, currentSide)
//      (move, score)
//    })
//
//    Some(scoredMoves.maxBy(_._2)._1)
//  }
//
//  private def evaluateMove(board: ChessBoard, move: ChessMove, side: Side): Int = {
//    val newBoard = board.applyMove(move)
//
//    // Calculate immediate capture value
//    val captureScore = evaluateCapture(board, move)
//
//    // If this is a capture that loses material, heavily penalize it
//    if (captureScore < 0) {
//      return captureScore * 100 // Strong penalty for bad trades
//    }
//
//    // Evaluate piece safety in new position
//    val safetyScore = evaluateSafety(newBoard, move, side)
//
//    // King safety evaluation
//    val kingSafetyScore = evaluateKingSafety(newBoard, move, side)
//
//    // Basic positional score
//    val positionScore = evaluatePosition(move, side)
//
//    captureScore * 10 + safetyScore * 50 + kingSafetyScore * 30 + positionScore
//  }
//
//  private def evaluateCapture(board: ChessBoard, move: ChessMove): Int = {
//    board.getPiece(move.toIndex) match {
//      case Some(piece) if piece.side != move.piece.side =>
//        pieceValues(piece.pieceType) - pieceValues(move.piece.pieceType)
//      case _ => 0
//    }
//  }
//
//  private def evaluateSafety(board: ChessBoard, move: ChessMove, side: Side): Int = {
//    var score = 0
//
//    // Penalty for moving to an attacked square
//    if (board.isSquareAttacked(move.toIndex, side.opposite)) {
//      score -= pieceValues(move.piece.pieceType)
//    }
//
//    // Extra penalty for moving valuable pieces where they can be captured by less valuable pieces
//    if (board.isSquareAttacked(move.toIndex, side.opposite)) {
//      val attackers       = findAttackers(board, move.toIndex, side.opposite)
//      val movedPieceValue = pieceValues(move.piece.pieceType)
//
//      attackers.foreach { attacker =>
//        if (pieceValues(attacker.pieceType) < movedPieceValue) {
//          score -= (movedPieceValue - pieceValues(attacker.pieceType)) * 2
//        }
//      }
//    }
//
//    score
//  }
//
//  private def evaluateKingSafety(board: ChessBoard, move: ChessMove, side: Side): Int = {
//    if (move.piece.pieceType != PieceType.KING) return 0
//
//    var score  = 0
//    val toRank = move.toIndex / 8
//    val toFile = move.toIndex % 8
//
//    // Strong penalty for moving king early in the game
//    val isEarlyGame = isProbablyEarlyGame(board)
//    if (isEarlyGame) {
//      score -= 200 // Heavy penalty for early king moves
//    }
//
//    // Penalty for moving to center files in early/middle game
//    val centerFileDistance = math.min(math.abs(toFile - 3), math.abs(toFile - 4))
//    if (centerFileDistance < 2) {
//      score -= (2 - centerFileDistance) * 50
//    }
//
//    // Penalty for exposed king
//    val attackedSquaresAroundKing = countAttackedSquaresAroundKing(board, move.toIndex, side)
//    score -= attackedSquaresAroundKing * 30
//
//    score
//  }
//
//  private def evaluatePosition(move: ChessMove, side: Side): Int = {
//    val toSquare = move.toIndex
//    val file     = toSquare % 8
//    val rank     = toSquare / 8
//
//    var score = 0
//
//    // Center control
//    val centerFile = 3 - math.abs(file - 3.5).toInt
//    val centerRank = 3 - math.abs(rank - 3.5).toInt
//    score += (centerFile + centerRank) * 10
//
//    // Bonus for development in early game
//    if (isProbablyEarlyGame(move.piece, side)) {
//      score += 20 // Encourage piece development
//    }
//
//    score
//  }
//
//  private def isProbablyEarlyGame(board: ChessBoard): Boolean = {
//    // Count developed pieces
//    val developedPieces = board.getPieces.count { case (square, piece) =>
//      val rank = square / 8
//      piece.pieceType match {
//        case PieceType.KNIGHT | PieceType.BISHOP => rank != (if (piece.side == White) 0 else 7)
//        case _                                   => false
//      }
//    }
//
//    developedPieces < 6 // Consider it early game if less than 6 pieces are developed
//  }
//
//  private def isProbablyEarlyGame(piece: Piece, side: Side): Boolean = {
//    piece.pieceType match {
//      case PieceType.KNIGHT | PieceType.BISHOP => true
//      case _                                   => false
//    }
//  }
//
//  private def findAttackers(board: ChessBoard, square: Int, side: Side): List[Piece] = {
//    board.getPiecesBySide(side)
//      .filter { case (fromSquare, piece) =>
//        board.getMoves(side).exists(move =>
//          move.fromIndex == fromSquare && move.toIndex == square,
//        )
//      }
//      .map(_._2)
//      .toList
//  }
//
//  private def countAttackedSquaresAroundKing(board: ChessBoard, kingSquare: Int, side: Side): Int = {
//    val file = kingSquare % 8
//    val rank = kingSquare / 8
//
//    val adjacentSquares = for {
//      f <- math.max(0, file - 1) to math.min(7, file + 1)
//      r <- math.max(0, rank - 1) to math.min(7, rank + 1)
//      if f != file || r != rank
//    } yield r * 8 + f
//
//    adjacentSquares.count(square => board.isSquareAttacked(square, side.opposite))
//  }
//}

//package io.github.edadma.chess
//
//class SimpleEngine extends Engine {
//  // Basic piece values
//  private val pieceValues = Map[PieceType, Int](
//    PieceType.PAWN   -> 100,
//    PieceType.KNIGHT -> 300,
//    PieceType.BISHOP -> 300,
//    PieceType.ROOK   -> 500,
//    PieceType.QUEEN  -> 900,
//    PieceType.KING   -> 10000,
//  )
//
//  def makeMove(game: Game): Option[ChessMove] = {
//    val moves = game.getBoard.getMoves(game.getCurrentTurn).toList
//    if (moves.isEmpty) return None
//
//    // First, check for immediate checkmate
//    val mateMoves = moves.filter(move => {
//      val newBoard = game.getBoard.applyMove(move)
//      newBoard.isCheckmate(game.getCurrentTurn.opposite)
//    })
//
//    if (mateMoves.nonEmpty) return Some(mateMoves.head)
//
//    // Evaluate each move
//    val scoredMoves = moves.map(move => {
//      val score = evaluateMove(game.getBoard, move, game.getCurrentTurn)
//      (move, score)
//    })
//
//    // Choose the move with the highest score
//    Some(scoredMoves.maxBy(_._2)._1)
//  }
//
//  private def evaluateMove(board: ChessBoard, move: ChessMove, side: Side): Int = {
//    val newBoard = board.applyMove(move)
//
//    // Calculate immediate capture value
//    val captureScore = evaluateCapture(board, move)
//
//    // If this is a capture that loses material, heavily penalize it
//    if (captureScore < 0) {
//      return captureScore * 100 // Strong penalty for bad trades
//    }
//
//    // Check if the moved piece can be captured
//    val safetyScore = if (newBoard.isSquareAttacked(move.toIndex, side.opposite)) {
//      val movedPieceValue = pieceValues(move.piece.pieceType)
//      -movedPieceValue // Penalize moving pieces where they can be captured
//    } else {
//      0
//    }
//
//    // Basic positional score - favor controlling center squares
//    val positionScore = evaluatePosition(move.toIndex)
//
//    captureScore * 10 + safetyScore * 5 + positionScore
//  }
//
//  private def evaluateCapture(board: ChessBoard, move: ChessMove): Int = {
//    // Get the captured piece if any
//    val capturedPiece = board.getPiece(move.toIndex)
//
//    capturedPiece match {
//      case Some(piece) if piece.side != move.piece.side =>
//        // This is a capture - evaluate the trade
//        val capturedValue  = pieceValues(piece.pieceType)
//        val attackingValue = pieceValues(move.piece.pieceType)
//
//        // If we're giving up a more valuable piece, this is a bad trade
//        capturedValue - attackingValue
//
//      case _ => 0 // No capture
//    }
//  }
//
//  private def evaluatePosition(square: Int): Int = {
//    val file = square % 8
//    val rank = square / 8
//
//    // Simple center control bonus
//    val centerFile = 3 - math.abs(file - 3.5).toInt
//    val centerRank = 3 - math.abs(rank - 3.5).toInt
//
//    centerFile * 10 + centerRank * 10
//  }
//}
