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

  private def getPieceSymbol(piece: Piece): String = piece.pieceType match {
    case PieceType.KING   => "K"
    case PieceType.QUEEN  => "Q"
    case PieceType.ROOK   => "R"
    case PieceType.BISHOP => "B"
    case PieceType.KNIGHT => "N"
    case _                => "" // Pawns don't have a symbol in SAN
  }

  private def getFileRankFromIndex(index: Int): (Char, Int) = {
    val file = ('a' + (index % 8)).toChar
    val rank = (index / 8) + 1
    (file, rank)
  }

  def lastMoveSAN: String = {
    val builder = new StringBuilder

    currentBoard.lastMove.get.moveType match {
      case MoveType.CASTLE_KINGSIDE =>
        builder.append("O-O")
      case MoveType.CASTLE_QUEENSIDE =>
        builder.append("O-O-O")
      case _ =>
        // Add piece symbol (except for pawns)
        if (currentBoard.lastMove.get.piece.pieceType != PieceType.PAWN) {
          builder.append(getPieceSymbol(currentBoard.lastMove.get.piece))
        }

        // Handle disambiguation
        val similarMoves = currentBoard.getMoves(currentTurn).filter(m =>
          m.piece == currentBoard.lastMove.get.piece &&
            m.toIndex == currentBoard.lastMove.get.toIndex &&
            m.fromIndex != currentBoard.lastMove.get.fromIndex,
        ).toList

        if (similarMoves.nonEmpty) {
          val (fromFile, fromRank) = getFileRankFromIndex(currentBoard.lastMove.get.fromIndex)
          val needRank             = similarMoves.exists(m => getFileRankFromIndex(m.fromIndex)._1 == fromFile)
          val needFile             = similarMoves.exists(m => getFileRankFromIndex(m.fromIndex)._2 == fromRank)

          if (needFile || !needRank) {
            builder.append(fromFile)
          }
          if (needRank) {
            builder.append(fromRank)
          }
        }

        // Add 'x' for captures
        val isCapture = currentBoard.getPiece(currentBoard.lastMove.get.toIndex).isDefined
        if (
          isCapture || (currentBoard.lastMove.get.piece.pieceType == PieceType.PAWN &&
            getFileRankFromIndex(currentBoard.lastMove.get.fromIndex)._1 != getFileRankFromIndex(
              currentBoard.lastMove.get.toIndex,
            )._1)
        ) {
          if (currentBoard.lastMove.get.piece.pieceType == PieceType.PAWN) {
            builder.append(getFileRankFromIndex(currentBoard.lastMove.get.fromIndex)._1)
          }
          builder.append('x')
        }

        // Add destination square
        val (toFile, toRank) = getFileRankFromIndex(currentBoard.lastMove.get.toIndex)
        builder.append(toFile)
        builder.append(toRank)

        // Add promotion if applicable
        currentBoard.lastMove.get.promotion.foreach(p => {
          builder.append('=')
          builder.append(getPieceSymbol(p))
        })
    }

    // Add check/checkmate symbols
    if (isCheckmate) {
      builder.append('#')
    } else if (isInCheck) {
      builder.append('+')
    }

    builder.toString
  }
}

sealed trait GameStatus
case class Ongoing(turn: Side)      extends GameStatus
case class Checkmate(winner: Side)  extends GameStatus
case class Draw(reason: DrawReason) extends GameStatus

enum DrawReason {
  case Stalemate, Repetition, FiftyMoveRule, InsufficientMaterial
}
