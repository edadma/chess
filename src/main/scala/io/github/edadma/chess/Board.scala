package io.github.edadma.chess

val A1 = "A1"; val B1 = "B1"; val C1 = "C1"; val D1 = "D1"; val E1 = "E1"; val F1 = "F1"; val G1 = "G1"; val H1 = "H1"
val A2 = "A2"; val B2 = "B2"; val C2 = "C2"; val D2 = "D2"; val E2 = "E2"; val F2 = "F2"; val G2 = "G2"; val H2 = "H2"
val A3 = "A3"; val B3 = "B3"; val C3 = "C3"; val D3 = "D3"; val E3 = "E3"; val F3 = "F3"; val G3 = "G3"; val H3 = "H3"
val A4 = "A4"; val B4 = "B4"; val C4 = "C4"; val D4 = "D4"; val E4 = "E4"; val F4 = "F4"; val G4 = "G4"; val H4 = "H4"
val A5 = "A5"; val B5 = "B5"; val C5 = "C5"; val D5 = "D5"; val E5 = "E5"; val F5 = "F5"; val G5 = "G5"; val H5 = "H5"
val A6 = "A6"; val B6 = "B6"; val C6 = "C6"; val D6 = "D6"; val E6 = "E6"; val F6 = "F6"; val G6 = "G6"; val H6 = "H6"
val A7 = "A7"; val B7 = "B7"; val C7 = "C7"; val D7 = "D7"; val E7 = "E7"; val F7 = "F7"; val G7 = "G7"; val H7 = "H7"
val A8 = "A8"; val B8 = "B8"; val C8 = "C8"; val D8 = "D8"; val E8 = "E8"; val F8 = "F8"; val G8 = "G8"; val H8 = "H8"

def fromAlgebraic(s: String): Option[Int] = {
  if (s.length != 2) return None

  val file = s(0).toLower - 'a'
  val rank = s(1).asDigit - 1

  if (file < 0 || file > 7 || rank < 0 || rank > 7) None
  else Some(rank * 8 + file)
}

// Convert board index (0-63) to algebraic notation
def toAlgebraic(square: Int): String = {
  require(square >= 0 && square < 64, "Square index must be between 0 and 63")
  val file = ('a' + square % 8).toChar
  val rank = (square / 8 + 1).toString
  file.toString + rank
}

  def fromString(layout: String, whiteToMove: Boolean = true): Board = {
    val rows = layout.trim.split('\n')
    require(rows.length == 8, "Board must have 8 rows")

    var whitePawns, whiteKnights, whiteBishops, whiteRooks, whiteQueens, whiteKing = 0L
    var blackPawns, blackKnights, blackBishops, blackRooks, blackQueens, blackKing = 0L

    for {
      (row, rankIndex)       <- rows.zipWithIndex
      (pieceChar, fileIndex) <- row.trim.grouped(3).zipWithIndex if pieceChar.trim.nonEmpty
      square = (7 - rankIndex) * 8 + fileIndex
    } {
      val bit = 1L << square
      pieceChar(0) match {
        case 'P' => whitePawns |= bit
        case 'N' => whiteKnights |= bit
        case 'B' => whiteBishops |= bit
        case 'R' => whiteRooks |= bit
        case 'Q' => whiteQueens |= bit
        case 'K' => whiteKing |= bit
        case 'p' =>
          logger.debug(s"Found black pawn in source at rank=$rankIndex file=$fileIndex")
          logger.debug(s"Calculated square = ${(7 - rankIndex)} * 8 + $fileIndex = $square (${toAlgebraic(square)})")
          blackPawns |= bit
        case 'n' => blackKnights |= bit
        case 'b' => blackBishops |= bit
        case 'r' => blackRooks |= bit
        case 'q' => blackQueens |= bit
        case 'k' => blackKing |= bit
        case '.' => // Empty square
        case c   => throw new IllegalArgumentException(s"Invalid piece character: $c")
      }
    }

    Board(
      whitePawns = whitePawns,
      whiteKnights = whiteKnights,
      whiteBishops = whiteBishops,
      whiteRooks = whiteRooks,
      whiteQueens = whiteQueens,
      whiteKing = whiteKing,
      blackPawns = blackPawns,
      blackKnights = blackKnights,
      blackBishops = blackBishops,
      blackRooks = blackRooks,
      blackQueens = blackQueens,
      blackKing = blackKing,
    )
  }
}

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

  def promotionPiece: Option[Piece]
}

// Case class for moves
case class Move(
    from: Int,
    to: Int,
    piece: Piece,
    capture: Option[Piece] = None,
    promotion: Option[Piece] = None,
    isEnPassant: Boolean = false,
    isCastling: Boolean = false,
)

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
