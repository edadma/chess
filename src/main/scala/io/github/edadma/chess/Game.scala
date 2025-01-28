package io.github.edadma.chess

class Game(start: ChessBoard = Board()) {
  private var currentBoard: ChessBoard = start
  private var currentTurn: Side        = White
  private var moves: List[Move]        = List.empty
  private var halfMoveClock: Int       = 0

  def makeMove(move: Move): Boolean = {
    // Verify it's a legal move for the current side
    if (!currentBoard.getMoves(currentTurn).toList.contains(move)) {
      return false
    }

    // Apply the move
    currentBoard = currentBoard.applyMove(move)

    // Update move history
    moves = move :: moves

    // Update halfmove clock - reset on pawn moves or captures
    halfMoveClock =
      if (move.piece.pieceType == PieceType.PAWN || currentBoard.getPiece(move.toIndex).isDefined)
        0
      else
        halfMoveClock + 1

    // Switch turns
    currentTurn = currentTurn.opposite

    true
  }

  def getBoard: ChessBoard = currentBoard

  def getMoveHistory: List[Move] = moves

  def getCurrentTurn: Side = currentTurn

  def isGameOver: Boolean =
    isCheckmate || isStalemate || isDrawByRepetition || isDrawByFiftyMoveRule

  def isCheckmate: Boolean = currentBoard.isCheckmate(currentTurn)

  def isStalemate: Boolean = currentBoard.isStalemate(currentTurn)

  def isDrawByRepetition: Boolean = {
    // Count occurrences of each position in the move history
    val positions = moves.scanLeft(currentBoard)((b, m) => b.applyMove(m))
    positions.groupBy(_.getPieces).exists(_._2.size >= 3)
  }

  def isDrawByFiftyMoveRule: Boolean = halfMoveClock >= 100 // 50 moves = 100 half moves

  def isInsufficientMaterial: Boolean = {
    val pieces = currentBoard.getPieces.map(_._2).toList

    if (pieces.size <= 2) {
      // Just kings, or king and minor piece
      true
    } else if (pieces.size == 3) {
      // Check for king and bishop vs king or king and knight vs king
      val nonKings = pieces.filter(_.pieceType != PieceType.KING)
      nonKings.size == 1 &&
      (nonKings.head.pieceType == PieceType.BISHOP ||
        nonKings.head.pieceType == PieceType.KNIGHT)
    } else false
  }

  def status: GameStatus = {
    if (isCheckmate) Checkmate(currentTurn.opposite)
    else if (isStalemate) Draw(DrawReason.Stalemate)
    else if (isDrawByRepetition) Draw(DrawReason.Repetition)
    else if (isDrawByFiftyMoveRule) Draw(DrawReason.FiftyMoveRule)
    else if (isInsufficientMaterial) Draw(DrawReason.InsufficientMaterial)
    else Ongoing(currentTurn)
  }
}

sealed trait GameStatus
case class Ongoing(turn: Side)      extends GameStatus
case class Checkmate(winner: Side)  extends GameStatus
case class Draw(reason: DrawReason) extends GameStatus

enum DrawReason {
  case Stalemate, Repetition, FiftyMoveRule, InsufficientMaterial
}
