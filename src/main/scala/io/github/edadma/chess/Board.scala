package io.github.edadma.chess

val A1 = "A1"; val B1 = "B1"; val C1 = "C1"; val D1 = "D1"; val E1 = "E1"; val F1 = "F1"; val G1 = "G1"; val H1 = "H1"
val A2 = "A2"; val B2 = "B2"; val C2 = "C2"; val D2 = "D2"; val E2 = "E2"; val F2 = "F2"; val G2 = "G2"; val H2 = "H2"
val A3 = "A3"; val B3 = "B3"; val C3 = "C3"; val D3 = "D3"; val E3 = "E3"; val F3 = "F3"; val G3 = "G3"; val H3 = "H3"
val A4 = "A4"; val B4 = "B4"; val C4 = "C4"; val D4 = "D4"; val E4 = "E4"; val F4 = "F4"; val G4 = "G4"; val H4 = "H4"
val A5 = "A5"; val B5 = "B5"; val C5 = "C5"; val D5 = "D5"; val E5 = "E5"; val F5 = "F5"; val G5 = "G5"; val H5 = "H5"
val A6 = "A6"; val B6 = "B6"; val C6 = "C6"; val D6 = "D6"; val E6 = "E6"; val F6 = "F6"; val G6 = "G6"; val H6 = "H6"
val A7 = "A7"; val B7 = "B7"; val C7 = "C7"; val D7 = "D7"; val E7 = "E7"; val F7 = "F7"; val G7 = "G7"; val H7 = "H7"
val A8 = "A8"; val B8 = "B8"; val C8 = "C8"; val D8 = "D8"; val E8 = "E8"; val F8 = "F8"; val G8 = "G8"; val H8 = "H8"

def parseBoardString(board: String): Set[(Int, Int, Piece)] = {
  val lines = board.split("\n").map(_.trim).filter(_.nonEmpty)
  val pieces =
    for {
      (line, y) <- lines.zipWithIndex.toList
      chars = line.replaceAll("\\s+", "")
      (piece, x) <- chars.zipWithIndex
      if piece != '.'
    } yield (
      x,
      7 - y,
      piece match {
        case 'P' => WhitePawn
        case 'N' => WhiteKnight
        case 'B' => WhiteBishop
        case 'R' => WhiteRook
        case 'Q' => WhiteQueen
        case 'K' => WhiteKing
        case 'p' => BlackPawn
        case 'n' => BlackKnight
        case 'b' => BlackBishop
        case 'r' => BlackRook
        case 'q' => BlackQueen
        case 'k' => BlackKing
      },
    )

  pieces.toSet
}

trait ChessBoard {
  def getPiece(square: Int): Option[Piece]
  def applyMove(move: Move): ChessBoard
  def lastMove: Option[Move]
  def isLegalMove(move: Move): Boolean
  def getLegalMoves: Iterator[Move]
  def isInCheck(side: Side): Boolean
  def isCheckmate(side: Side): Boolean
  def isStalemate(side: Side): Boolean
  def isSquareAttacked(square: Int, by: Side): Boolean
}

case class Board(pieces: Map[String, Piece], lastMove: Option[Move]) extends ChessBoard:
  def getPiece(square: Int): Option[Piece] = pieces.get(toAlgebraic(square))
  def applyMove(move: Move): ChessBoard =
    val from  = toAlgebraic(move.fromIndex)
    val piece = pieces(from)
    val to    = toAlgebraic(move.toIndex)

    Board(pieces + (to -> piece) - from, Some(move))

enum PieceType {
  case KING, QUEEN, ROOK, BISHOP, KNIGHT, PAWN
}

enum MoveType {
  case NORMAL, EN_PASSANT, CASTLE_KINGSIDE, CASTLE_QUEENSIDE
}

trait ChessMove {
  def fromIndex: Int
  def toIndex: Int
  def piece: Piece
  def moveType: MoveType
  def promotion: Option[Piece]
}

case class Move(
    from: String,
    to: String,
    piece: Piece,
    moveType: MoveType,
    promotion: Option[Piece] = None,
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
