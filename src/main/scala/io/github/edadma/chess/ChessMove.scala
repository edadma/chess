package io.github.edadma.chess

enum PieceType {
  case KING, QUEEN, ROOK, BISHOP, KNIGHT, PAWN
}

enum MoveType {
  case NORMAL, EN_PASSANT, CASTLE_KINGSIDE, CASTLE_QUEENSIDE
}

trait ChessMoveFactory {
  def create(fromIndex: Int, toIndex: Int, piece: Piece, moveType: MoveType, promotion: Option[Piece]): ChessMove
}

object MoveFactory extends ChessMoveFactory {
  def create(fromIndex: Int, toIndex: Int, piece: Piece, moveType: MoveType, promotion: Option[Piece]): Move =
    Move(toAlgebraic(fromIndex), toAlgebraic(toIndex), piece, moveType, promotion)
}

trait ChessMove {
  def fromIndex: Int
  def toIndex: Int
  def piece: Piece
  def moveType: MoveType
  def promotion: Option[Piece]

  override def equals(other: Any): Boolean = other match {
    case that: ChessMove =>
      this.fromIndex == that.fromIndex &&
      this.toIndex == that.toIndex
    case _ => false
  }

  override def hashCode(): Int = {
    41 * (41 + fromIndex) + toIndex
  }
}

class Move(
    from: String,
    to: String,
    val piece: Piece,
    val moveType: MoveType = MoveType.NORMAL,
    val promotion: Option[Piece] = None,
) extends ChessMove:
  def fromIndex: Int = fromAlgebraic(from)
  def toIndex: Int   = fromAlgebraic(to)

case class BitMove(
    fromIndex: Int,
    toIndex: Int,
    piece: Piece,
    moveType: MoveType,
    promotion: Option[Piece] = None,
) extends ChessMove

sealed trait Side {
  def opposite: Side
}

case object White extends Side {
  def opposite: Side = Black
}

case object Black extends Side {
  def opposite: Side = White
}

sealed trait Piece {
  def side: Side
  def pieceType: PieceType
}

class UserMove(from: String, to: String, board: ChessBoard) extends ChessMove {
  val fromIndex: Int = fromAlgebraic(from)

  val toIndex: Int = fromAlgebraic(to)

  val piece: Piece = board.getPiece(fromIndex).getOrElse(
    throw new IllegalStateException(s"No piece found at square $from"),
  )

  val moveType: MoveType = MoveType.NORMAL

  val promotion: Option[Piece] = None
}

case object WhitePawn   extends Piece { def side: Side = White; def pieceType: PieceType = PieceType.PAWN   }
case object WhiteKnight extends Piece { def side: Side = White; def pieceType: PieceType = PieceType.KNIGHT }
case object WhiteBishop extends Piece { def side: Side = White; def pieceType: PieceType = PieceType.BISHOP }
case object WhiteRook   extends Piece { def side: Side = White; def pieceType: PieceType = PieceType.ROOK   }
case object WhiteQueen  extends Piece { def side: Side = White; def pieceType: PieceType = PieceType.QUEEN  }
case object WhiteKing   extends Piece { def side: Side = White; def pieceType: PieceType = PieceType.KING   }

case object BlackPawn   extends Piece { def side: Side = Black; def pieceType: PieceType = PieceType.PAWN   }
case object BlackKnight extends Piece { def side: Side = Black; def pieceType: PieceType = PieceType.KNIGHT }
case object BlackBishop extends Piece { def side: Side = Black; def pieceType: PieceType = PieceType.BISHOP }
case object BlackRook   extends Piece { def side: Side = Black; def pieceType: PieceType = PieceType.ROOK   }
case object BlackQueen  extends Piece { def side: Side = Black; def pieceType: PieceType = PieceType.QUEEN  }
case object BlackKing   extends Piece { def side: Side = Black; def pieceType: PieceType = PieceType.KING   }
