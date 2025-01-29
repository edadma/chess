package io.github.edadma.chess

class Game(start: ChessBoard = Board()) {
  private var currentBoard: ChessBoard = start
  private var currentTurn: Side        = White
  private var moves: List[ChessMove]   = List.empty
  private var halfMoveClock: Int       = 0

  def moveFactory: ChessMoveFactory = currentBoard.moveFactory

  def makeMove(move: ChessMove): Boolean = {
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

  def getMoveHistory: List[ChessMove] = moves

  def getCurrentTurn: Side = currentTurn

  def boardToString(side: Side): String = currentBoard.boardToString(side)

  def isGameOver: Boolean =
    isCheckmate || isStalemate || isDrawByRepetition || isDrawByFiftyMoveRule

  def isInCheck: Boolean = currentBoard.isInCheck(currentTurn)

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

  def lastMoveToSAN: String = {
    moves.headOption.map(move => moveToSAN(move, moves.tail.headOption)).getOrElse("")
  }

  private def moveToSAN(move: ChessMove, previousMove: Option[ChessMove]): String = {
    val builder = new StringBuilder
    val previousBoard = previousMove match {
      case Some(prevMove) => moves.tail.foldLeft(start)((b, m) => if (m != prevMove) b.applyMove(m) else b)
      case None           => if (moves.length > 1) moves.tail.foldLeft(start)((b, m) => b.applyMove(m)) else start
    }

    move.moveType match {
      case MoveType.CASTLE_KINGSIDE  => builder.append("O-O")
      case MoveType.CASTLE_QUEENSIDE => builder.append("O-O-O")
      case _ =>
        if (move.piece.pieceType != PieceType.PAWN) {
          builder.append(pieceToChar(move.piece))
          // Disambiguation
          val otherPieces = previousBoard.getPiecesBySide(move.piece.side).filter { case (square, piece) =>
            piece.pieceType == move.piece.pieceType && square != move.fromIndex &&
            previousBoard.getMoves(move.piece.side).exists(m => m.fromIndex == square && m.toIndex == move.toIndex)
          }
          if (otherPieces.nonEmpty) {
            val fromFile = move.fromIndex % 8
            val fromRank = move.fromIndex / 8
            if (otherPieces.exists(_._1 % 8 == fromFile)) {
              builder.append((fromRank + '1').toChar)
            } else {
              builder.append((fromFile + 'a').toChar)
            }
          }
        }

        // Handle captures
        val isCapture = previousBoard.getPiece(move.toIndex).isDefined ||
          (move.piece.pieceType == PieceType.PAWN && (move.fromIndex % 8) != (move.toIndex % 8))
        if (isCapture) {
          if (move.piece.pieceType == PieceType.PAWN) {
            builder.append((move.fromIndex % 8 + 'a').toChar)
          }
          builder.append('x')
        }

        // Destination square
        builder.append((move.toIndex % 8 + 'a').toChar)
        builder.append((move.toIndex / 8 + 1).toChar)

        // Pawn promotion
        move.promotion.foreach(p => {
          builder.append('=')
          builder.append(pieceToChar(p))
        })
    }

    // Check or checkmate
    val afterBoard = previousBoard.applyMove(move)
    if (afterBoard.isCheckmate(move.piece.side.opposite)) {
      builder.append('#')
    } else if (afterBoard.isInCheck(move.piece.side.opposite)) {
      builder.append('+')
    }

    builder.toString
  }

  private def pieceToChar(piece: Piece): Char = piece.pieceType match {
    case PieceType.KING   => 'K'
    case PieceType.QUEEN  => 'Q'
    case PieceType.ROOK   => 'R'
    case PieceType.BISHOP => 'B'
    case PieceType.KNIGHT => 'N'
    case PieceType.PAWN   => ' '
  }
}

sealed trait GameStatus
case class Ongoing(turn: Side)      extends GameStatus
case class Checkmate(winner: Side)  extends GameStatus
case class Draw(reason: DrawReason) extends GameStatus

enum DrawReason {
  case Stalemate, Repetition, FiftyMoveRule, InsufficientMaterial
}
