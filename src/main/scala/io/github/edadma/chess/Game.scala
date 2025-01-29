package io.github.edadma.chess

class Game(start: ChessBoard = Board()) {
  private var boards: List[ChessBoard] = List(start)
  private var currentTurn: Side        = White

  def currentBoard: ChessBoard          = boards.head
  def boardHistory: List[ChessBoard]    = boards.reverse
  def previousBoard: Option[ChessBoard] = boards.tail.headOption

  def moveFactory: ChessMoveFactory = currentBoard.moveFactory

  def makeMove(move: ChessMove): Boolean = {
    if (!currentBoard.getMoves(currentTurn).toList.contains(move)) {
      return false
    }

    val newBoard = currentBoard.applyMove(move)
    boards = newBoard :: boards
    currentTurn = currentTurn.opposite
    true
  }

  def getBoard: ChessBoard = currentBoard

  def getCurrentTurn: Side = currentTurn

  def boardToString(side: Side): String = currentBoard.boardToString(side)

  def isGameOver: Boolean =
    isCheckmate || isStalemate || isDrawByRepetition || isDrawByFiftyMoveRule

  def isInCheck: Boolean = currentBoard.isInCheck(currentTurn)

  def isCheckmate: Boolean = currentBoard.isCheckmate(currentTurn)

  def isStalemate: Boolean = currentBoard.isStalemate(currentTurn)

  def isDrawByRepetition: Boolean = {
    // Group the board states by their piece positions and count occurrences
    val positions = boards.map(board => board.getPieces.toSet).groupBy(identity)

    // Check if any position occurs 3 or more times
    positions.exists(_._2.size >= 3)
  }

  def isDrawByFiftyMoveRule: Boolean = {
    if (boards.size < 100) return false // Need at least 100 half-moves (50 full moves)

    // Look at the last 100 board states (50 moves)
    val recentBoards = boards.take(100)

    // Check for any pawn moves or captures in the sequence
    val noPawnMoveOrCapture = recentBoards.sliding(2).forall { pair =>
      val newer = pair.head
      val older = pair.last

      // Get the pieces for both boards
      val newerPieces = newer.getPieces.toMap
      val olderPieces = older.getPieces.toMap

      // Check if the moving piece was a pawn
      val lastMove       = newer.lastMove.get
      val wasNotPawnMove = lastMove.piece.pieceType != PieceType.PAWN

      // Check if there was a capture (piece count changed)
      val wasNotCapture = newerPieces.size == olderPieces.size

      wasNotPawnMove && wasNotCapture
    }

    noPawnMoveOrCapture
  }

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
    // Need at least 2 boards to get a move
    if (boards.size < 2) return ""

    val curBoard  = currentBoard
    val prevBoard = previousBoard.get
    val lastMove  = curBoard.lastMove.get

    // Handle castling first
    if (lastMove.moveType == MoveType.CASTLE_KINGSIDE) return "O-O" + getCheckSuffix(curBoard, lastMove)
    if (lastMove.moveType == MoveType.CASTLE_QUEENSIDE) return "O-O-O" + getCheckSuffix(curBoard, lastMove)

    val piece = lastMove.piece
    val pieceStr = piece.pieceType match {
      case PieceType.PAWN   => ""
      case PieceType.KNIGHT => "N"
      case PieceType.BISHOP => "B"
      case PieceType.ROOK   => "R"
      case PieceType.QUEEN  => "Q"
      case PieceType.KING   => "K"
    }

    // Get disambiguation if needed
    val disambig = getDisambiguation(prevBoard, lastMove)

    // Check if move was a capture
    val isCapture = prevBoard.getPiece(lastMove.toIndex).isDefined ||
      lastMove.moveType == MoveType.EN_PASSANT
    val captureStr = if (isCapture) "x" else ""

    // For pawns, include file when capturing
    val pawnCapturePrefix = if (piece.pieceType == PieceType.PAWN && isCapture)
      toAlgebraic(lastMove.fromIndex)(0).toString
    else ""

    // Handle promotions
    val promotionStr = lastMove.promotion.map(p => "=" + pieceToChar(p)).getOrElse("")

    // Build final SAN string
    pieceStr + disambig + pawnCapturePrefix + captureStr + toAlgebraic(lastMove.toIndex) +
      promotionStr + getCheckSuffix(curBoard, lastMove)
  }

  private def getDisambiguation(board: ChessBoard, move: ChessMove): String = {
    if (move.piece.pieceType == PieceType.PAWN) return ""

    // Find all pieces of same type that could move to same square
    val otherMoves = board.getMoves(move.piece.side).filter(m =>
      m.piece.pieceType == move.piece.pieceType &&
        m.toIndex == move.toIndex &&
        m.fromIndex != move.fromIndex,
    ).toList

    if (otherMoves.isEmpty) return ""

    val moveFile = move.fromIndex % 8
    val moveRank = move.fromIndex / 8

    // Check if file is sufficient for disambiguation
    val needRank = otherMoves.exists(m => m.fromIndex % 8 == moveFile)

    // If file is sufficient, just use file
    if (!needRank) {
      ('a' + moveFile).toChar.toString
    } else {
      // Otherwise use rank or both file and rank
      val needFile = otherMoves.exists(m => m.fromIndex / 8 == moveRank)
      if (needFile)
        toAlgebraic(move.fromIndex)
      else
        (moveRank + 1).toString
    }
  }

  private def getCheckSuffix(board: ChessBoard, move: ChessMove): String = {
    if (board.isCheckmate(move.piece.side.opposite)) "#"
    else if (board.isInCheck(move.piece.side.opposite)) "+"
    else ""
  }

  private def pieceToChar(piece: Piece): String = piece match {
    case WhiteQueen | BlackQueen   => "Q"
    case WhiteRook | BlackRook     => "R"
    case WhiteBishop | BlackBishop => "B"
    case WhiteKnight | BlackKnight => "N"
    case _                         => "Q" // Default case, should never happen
  }
}

sealed trait GameStatus
case class Ongoing(turn: Side)      extends GameStatus
case class Checkmate(winner: Side)  extends GameStatus
case class Draw(reason: DrawReason) extends GameStatus

enum DrawReason {
  case Stalemate, Repetition, FiftyMoveRule, InsufficientMaterial
}
