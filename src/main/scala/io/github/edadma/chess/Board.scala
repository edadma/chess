package io.github.edadma.chess

import io.github.edadma.chess.ChessBoard.knightOffsets

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

val A1 = "A1"; val B1 = "B1"; val C1 = "C1"; val D1 = "D1"; val E1 = "E1"; val F1 = "F1"; val G1 = "G1"; val H1 = "H1"
val A2 = "A2"; val B2 = "B2"; val C2 = "C2"; val D2 = "D2"; val E2 = "E2"; val F2 = "F2"; val G2 = "G2"; val H2 = "H2"
val A3 = "A3"; val B3 = "B3"; val C3 = "C3"; val D3 = "D3"; val E3 = "E3"; val F3 = "F3"; val G3 = "G3"; val H3 = "H3"
val A4 = "A4"; val B4 = "B4"; val C4 = "C4"; val D4 = "D4"; val E4 = "E4"; val F4 = "F4"; val G4 = "G4"; val H4 = "H4"
val A5 = "A5"; val B5 = "B5"; val C5 = "C5"; val D5 = "D5"; val E5 = "E5"; val F5 = "F5"; val G5 = "G5"; val H5 = "H5"
val A6 = "A6"; val B6 = "B6"; val C6 = "C6"; val D6 = "D6"; val E6 = "E6"; val F6 = "F6"; val G6 = "G6"; val H6 = "H6"
val A7 = "A7"; val B7 = "B7"; val C7 = "C7"; val D7 = "D7"; val E7 = "E7"; val F7 = "F7"; val G7 = "G7"; val H7 = "H7"
val A8 = "A8"; val B8 = "B8"; val C8 = "C8"; val D8 = "D8"; val E8 = "E8"; val F8 = "F8"; val G8 = "G8"; val H8 = "H8"

def parseBoardString(board: String): Set[(Int, Piece)] = {
  val lines = board.split("\n").map(_.trim).filter(_.nonEmpty)
  val pieces =
    for {
      (line, y) <- lines.zipWithIndex.toList
      chars = line.replaceAll("\\s+", "")
      (piece, x) <- chars.zipWithIndex
      if piece != '.'
    } yield (
      (7 - y) * 8 + x,
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

object ChessBoard {
  private val knightOffsets = List(
    (-2, -1),
    (-2, 1),
    (-1, -2),
    (-1, 2),
    (1, -2),
    (1, 2),
    (2, -1),
    (2, 1),
  )

  private val kingOffsets = List(
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
  )

  private val rookDirections = List(
    (-1, 0), // up
    (1, 0),  // down
    (0, -1), // left
    (0, 1),  // right
  )

  private val bishopDirections = List(
    (-1, -1), // up-left
    (-1, 1),  // up-right
    (1, -1),  // down-left
    (1, 1),   // down-right
  )
}

trait ChessBoard {
  def getPieces: Iterator[(Int, Piece)]
  def getPiece(square: Int): Option[Piece]
  def getPiece(file: Int, rank: Int): Option[Piece]
  def getMoves(side: Side): Iterator[ChessMove]

  def canCastleKingside(side: Side): Boolean

  def canCastleQueenside(side: Side): Boolean

  protected def getCastlingMoves(side: Side, factory: ChessMoveFactory): Iterator[ChessMove] = {
    if (!isInCheck(side)) {
      val rank    = if (side == White) 0 else 7
      val kingPos = rank * 8 + 4
      val moves   = new ListBuffer[ChessMove]()

      // Kingside castling
      if (
        canCastleKingside(side) &&
        getPiece(rank * 8 + 5).isEmpty &&
        getPiece(rank * 8 + 6).isEmpty &&
        !isSquareAttacked(rank * 8 + 5, side.opposite) &&
        !isSquareAttacked(rank * 8 + 6, side.opposite)
      ) {
        moves += factory.create(kingPos, rank * 8 + 6, getPiece(kingPos).get, MoveType.CASTLE_KINGSIDE, None)
      }

      // Queenside castling
      if (
        canCastleQueenside(side) &&
        getPiece(rank * 8 + 3).isEmpty &&
        getPiece(rank * 8 + 2).isEmpty &&
        getPiece(rank * 8 + 1).isEmpty &&
        !isSquareAttacked(rank * 8 + 3, side.opposite) &&
        !isSquareAttacked(rank * 8 + 2, side.opposite)
      ) {
        moves += factory.create(kingPos, rank * 8 + 2, getPiece(kingPos).get, MoveType.CASTLE_QUEENSIDE, None)
      }

      moves.iterator
    } else Iterator.empty
  }

  def getPiecesBySide(side: Side): Iterator[(Int, Piece)] = {
    getPieces.filter(_._2.side == side)
  }

  def getPiecesByType(pieceTypes: Set[PieceType], side: Side): Iterator[Int] = {
    getPiecesBySide(side).filter((_, p) => pieceTypes(p.pieceType)).map(_._1)
  }

  def isInCheck(side: Side): Boolean =
    getPiecesByType(Set(PieceType.KING), side).exists(square => isSquareAttacked(square, side.opposite))

  protected def getKnightMoveSquares(fromSquare: Int): Iterator[Int] = {
    val fromRank = fromSquare / 8
    val fromFile = fromSquare % 8

    ChessBoard.knightOffsets.iterator.map { case (rankOffset, fileOffset) =>
      val toRank = fromRank + rankOffset
      val toFile = fromFile + fileOffset
      (toRank, toFile)
    }.filter { case (rank, file) =>
      rank >= 0 && rank < 8 && file >= 0 && file < 8
    }.map { case (rank, file) =>
      rank * 8 + file
    }
  }

  protected def getKnightMoves(side: Side, factory: ChessMoveFactory): Iterator[ChessMove] = {
    getPiecesByType(Set(PieceType.KNIGHT), side).flatMap { fromSquare =>
      getKnightMoveSquares(fromSquare)
        .filterNot(toSquare => getPiece(toSquare).exists(_.side == side))
        .map(toSquare =>
          factory.create(fromSquare, toSquare, getPiece(fromSquare).get, MoveType.NORMAL, None),
        )
    }
  }

  protected def isAttackedByKnight(targetSquare: Int, attackingSide: Side): Boolean = {
    if (getPiece(targetSquare).exists(_.side == attackingSide)) false
    else getKnightMoveSquares(targetSquare).exists(square =>
      getPiece(square).exists(p => p.side == attackingSide && p.pieceType == PieceType.KNIGHT),
    )
  }

  protected def getKingMoveSquares(fromSquare: Int): Iterator[Int] = {
    val fromRank = fromSquare / 8
    val fromFile = fromSquare % 8

    ChessBoard.kingOffsets.iterator.map { case (rankOffset, fileOffset) =>
      val toRank = fromRank + rankOffset
      val toFile = fromFile + fileOffset
      (toRank, toFile)
    }.filter { case (rank, file) =>
      rank >= 0 && rank < 8 && file >= 0 && file < 8
    }.map { case (rank, file) =>
      rank * 8 + file
    }
  }

  protected def getKingMoves(side: Side, factory: ChessMoveFactory): Iterator[ChessMove] = {
    getPiecesByType(Set(PieceType.KING), side).flatMap { fromSquare =>
      getKingMoveSquares(fromSquare)
        .filterNot(toSquare => getPiece(toSquare).exists(_.side == side))
        .map(toSquare =>
          factory.create(fromSquare, toSquare, getPiece(fromSquare).get, MoveType.NORMAL, None),
        )
    }
  }

  protected def isAttackedByKing(targetSquare: Int, attackingSide: Side): Boolean = {
    if (getPiece(targetSquare).exists(_.side == attackingSide)) false
    else getKingMoveSquares(targetSquare).exists(square =>
      getPiece(square).exists(p => p.side == attackingSide && p.pieceType == PieceType.KING),
    )
  }

  protected[chess] def ray(fromSquare: Int, fileDirection: Int, rankDirection: Int): Iterator[Int] = {
    val fromRank = fromSquare / 8
    val fromFile = fromSquare % 8

    new Iterator[Int] {
      private var curFile = fromFile
      private var curRank = fromRank
      private var hit     = false

      def hasNext: Boolean =
        curFile += fileDirection
        curRank += rankDirection
        curFile >= 0 && curFile <= 7 && curRank >= 0 && curRank <= 7 && !hit

      def next: Int =
        val res = curRank * 8 + curFile

        hit = getPiece(res).nonEmpty
        res
    }
  }

  protected[chess] def getRookMoveSquares(fromSquare: Int): Iterator[Int] = {
    ChessBoard.rookDirections.iterator.flatMap { case (rankDelta, fileDelta) =>
      ray(fromSquare, fileDelta, rankDelta)
    }
  }

  protected def getRookMoves(side: Side, factory: ChessMoveFactory): Iterator[ChessMove] = {
    getPiecesByType(Set(PieceType.ROOK, PieceType.QUEEN), side).flatMap { fromSquare =>
      getRookMoveSquares(fromSquare)
        .filter(toSquare =>
          getPiece(toSquare).forall(_.side != side),
        )
        .map(toSquare =>
          factory.create(fromSquare, toSquare, getPiece(fromSquare).get, MoveType.NORMAL, None),
        )
    }
  }

  protected def isAttackedByRook(targetSquare: Int, attackingSide: Side): Boolean = {
    if (getPiece(targetSquare).exists(_.side == attackingSide)) false
    else getRookMoveSquares(targetSquare).exists { square =>
      val piece = getPiece(square)
      piece.exists(p => p.side == attackingSide && (p.pieceType == PieceType.ROOK || p.pieceType == PieceType.QUEEN))
    }
  }

  protected def getBishopMoves(side: Side, factory: ChessMoveFactory): Iterator[ChessMove] = {
    getPiecesByType(Set(PieceType.BISHOP, PieceType.QUEEN), side).flatMap { fromSquare =>
      getBishopMoveSquares(fromSquare)
        .filter(toSquare =>
          getPiece(toSquare).forall(_.side != side),
        )
        .map(toSquare =>
          factory.create(fromSquare, toSquare, getPiece(fromSquare).get, MoveType.NORMAL, None),
        )
    }
  }

  protected def isAttackedByBishop(targetSquare: Int, attackingSide: Side): Boolean = {
    if (getPiece(targetSquare).exists(_.side == attackingSide)) false
    else getBishopMoveSquares(targetSquare).exists { square =>
      val piece = getPiece(square)
      piece.exists(p => p.side == attackingSide && (p.pieceType == PieceType.BISHOP || p.pieceType == PieceType.QUEEN))
    }
  }

  protected[chess] def getBishopMoveSquares(fromSquare: Int): Iterator[Int] = {
    ChessBoard.bishopDirections.iterator.flatMap { case (rankDelta, fileDelta) =>
      ray(fromSquare, fileDelta, rankDelta)
    }
  }

  protected def getPawnMoves(side: Side, factory: ChessMoveFactory): Iterator[ChessMove] = {
    val direction     = if (side == White) 1 else -1
    val startRank     = if (side == White) 1 else 6
    val promotionRank = if (side == White) 7 else 0

    getPiecesByType(Set(PieceType.PAWN), side).flatMap { fromSquare =>
      val fromRank = fromSquare / 8
      val fromFile = fromSquare % 8
      val moves    = new ListBuffer[ChessMove]()

      // Single square advance
      val oneSquare = fromSquare + (direction * 8)
      if (oneSquare >= 0 && oneSquare < 64 && getPiece(oneSquare).isEmpty) {
        if (fromRank + direction == promotionRank) {
          // Pawn promotion
          List(WhiteQueen, WhiteRook, WhiteBishop, WhiteKnight).foreach { piece =>
            val promotionPiece = piece match {
              case WhiteQueen | BlackQueen   => if (side == White) WhiteQueen else BlackQueen
              case WhiteRook | BlackRook     => if (side == White) WhiteRook else BlackRook
              case WhiteBishop | BlackBishop => if (side == White) WhiteBishop else BlackBishop
              case WhiteKnight | BlackKnight => if (side == White) WhiteKnight else BlackKnight
              case _                         => WhiteQueen // Default case - should never happen
            }
            moves += factory.create(
              fromSquare,
              oneSquare,
              getPiece(fromSquare).get,
              MoveType.NORMAL,
              Some(promotionPiece),
            )
          }
        } else {
          moves += factory.create(fromSquare, oneSquare, getPiece(fromSquare).get, MoveType.NORMAL, None)
        }

        // Two square advance from starting position
        if (fromRank == startRank) {
          val twoSquares = fromSquare + (direction * 16)
          if (getPiece(twoSquares).isEmpty) {
            moves += factory.create(fromSquare, twoSquares, getPiece(fromSquare).get, MoveType.NORMAL, None)
          }
        }
      }

      // Captures
      for (fileOffset <- List(-1, 1)) {
        val captureSquare = fromSquare + (direction * 8) + fileOffset
        if (captureSquare >= 0 && captureSquare < 64) {
          val captureFile = captureSquare % 8
          if (math.abs(captureFile - fromFile) == 1) {
            // Normal capture
            getPiece(captureSquare).foreach { piece =>
              if (piece.side != side) {
                if (fromRank + direction == promotionRank) {
                  // Capture with promotion
                  List(WhiteQueen, WhiteRook, WhiteBishop, WhiteKnight).foreach { piece =>
                    val promotionPiece = piece match {
                      case WhiteQueen | BlackQueen   => if (side == White) WhiteQueen else BlackQueen
                      case WhiteRook | BlackRook     => if (side == White) WhiteRook else BlackRook
                      case WhiteBishop | BlackBishop => if (side == White) WhiteBishop else BlackBishop
                      case WhiteKnight | BlackKnight => if (side == White) WhiteKnight else BlackKnight
                      case _                         => WhiteQueen // Default case - should never happen
                    }
                    moves += factory.create(
                      fromSquare,
                      captureSquare,
                      getPiece(fromSquare).get,
                      MoveType.NORMAL,
                      Some(promotionPiece),
                    )
                  }
                } else {
                  moves += factory.create(fromSquare, captureSquare, getPiece(fromSquare).get, MoveType.NORMAL, None)
                }
              }
            }

            // En passant
            lastMove.foreach { move =>
              if (
                move.piece.pieceType == PieceType.PAWN &&
                math.abs(move.fromIndex - move.toIndex) == 16 &&
                move.toIndex % 8 == captureFile &&
                move.toIndex / 8 == fromRank
              ) {
                moves += factory.create(fromSquare, captureSquare, getPiece(fromSquare).get, MoveType.EN_PASSANT, None)
              }
            }
          }
        }
      }

      moves.iterator
    }
  }

  protected def isAttackedByPawn(targetSquare: Int, attackingSide: Side): Boolean = {
    val direction  = if (attackingSide == White) 1 else -1
    val targetFile = targetSquare % 8
    var isAttacked = false

    for (fileOffset <- List(-1, 1) if !isAttacked) {
      val attackerSquare = targetSquare - (direction * 8) + fileOffset
      if (attackerSquare >= 0 && attackerSquare < 64) {
        val attackerFile = attackerSquare % 8
        if (math.abs(attackerFile - targetFile) == 1) {
          getPiece(attackerSquare).foreach { piece =>
            if (piece.side == attackingSide && piece.pieceType == PieceType.PAWN) {
              isAttacked = true
            }
          }
        }
      }
    }
    isAttacked
  }

  def getMoves(side: Side, moveFactory: ChessMoveFactory): Iterator[ChessMove] =
    (getKnightMoves(side, moveFactory) ++ getKingMoves(side, moveFactory) ++ getRookMoves(side, moveFactory)
      ++ getBishopMoves(side, moveFactory) ++ getPawnMoves(side, moveFactory) ++
      getCastlingMoves(side, moveFactory))
      .filterNot(move => applyMove(move).isInCheck(side))

  def isSquareAttacked(square: Int, by: Side): Boolean =
    isAttackedByKnight(square, by) || isAttackedByKing(square, by) || isAttackedByRook(square, by)
      || isAttackedByBishop(square, by) || isAttackedByPawn(square, by)

  def isCheckmate(side: Side): Boolean = isInCheck(side) && getMoves(side).isEmpty

  def isStalemate(side: Side): Boolean = !isInCheck(side) && getMoves(side).isEmpty

  def applyMove(move: ChessMove): ChessBoard
  def lastMove: Option[Move]
}

object Board {
  def apply(): Board =
    Board.fromString(
      """
       r  n  b  q  k  b  n  r
       p  p  p  p  p  p  p  p
       .  .  .  .  .  .  .  .
       .  .  .  .  .  .  .  .
       .  .  .  .  .  .  .  .
       .  .  .  .  .  .  .  .
       P  P  P  P  P  P  P  P
       R  N  B  Q  K  B  N  R
     """,
    )

  def fromString(board: String): Board =
    val pieces = parseBoardString(board) map ((square, piece) => toAlgebraic(square) -> piece) toMap

    // Determine initial castling rights based on piece positions
    val whiteKingOnE1 = pieces.get("e1").contains(WhiteKing)
    val whiteRookOnA1 = pieces.get("a1").contains(WhiteRook)
    val whiteRookOnH1 = pieces.get("h1").contains(WhiteRook)
    val blackKingOnE8 = pieces.get("e8").contains(BlackKing)
    val blackRookOnA8 = pieces.get("a8").contains(BlackRook)
    val blackRookOnH8 = pieces.get("h8").contains(BlackRook)

    Board(
      pieces,
      None,
      whiteCanCastleKingside = whiteKingOnE1 && whiteRookOnH1,
      whiteCanCastleQueenside = whiteKingOnE1 && whiteRookOnA1,
      blackCanCastleKingside = blackKingOnE8 && blackRookOnH8,
      blackCanCastleQueenside = blackKingOnE8 && blackRookOnA8,
    )
}

case class Board(
    pieces: Map[String, Piece],
    lastMove: Option[Move] = None,
    whiteCanCastleKingside: Boolean = true,
    whiteCanCastleQueenside: Boolean = true,
    blackCanCastleKingside: Boolean = true,
    blackCanCastleQueenside: Boolean = true,
) extends ChessBoard:
  override def getPieces: Iterator[(Int, Piece)] = {
    pieces.iterator.map { case (square, piece) =>
      (fromAlgebraic(square), piece)
    }
  }

  def getPiece(square: Int): Option[Piece] = pieces.get(toAlgebraic(square))

  def getPiece(file: Int, rank: Int): Option[Piece] = pieces.get(toAlgebraic(rank * 8 + file))

  def getMoves(side: Side): Iterator[ChessMove] = getMoves(side, MoveFactory)

  def applyMove(move: ChessMove): ChessBoard =
    val from  = toAlgebraic(move.fromIndex)
    val piece = pieces(from)
    val to    = toAlgebraic(move.toIndex)

    // Update castling rights
    val newWhiteKingside = whiteCanCastleKingside &&
      !(piece == WhiteKing || (piece == WhiteRook && from == "h1"))
    val newWhiteQueenside = whiteCanCastleQueenside &&
      !(piece == WhiteKing || (piece == WhiteRook && from == "a1"))
    val newBlackKingside = blackCanCastleKingside &&
      !(piece == BlackKing || (piece == BlackRook && from == "h8"))
    val newBlackQueenside = blackCanCastleQueenside &&
      !(piece == BlackKing || (piece == BlackRook && from == "a8"))

    // Handle actual castling moves
    val newPieces = move.moveType match {
      case MoveType.CASTLE_KINGSIDE =>
        val rookFrom = if (piece.side == White) "h1" else "h8"
        val rookTo   = if (piece.side == White) "f1" else "f8"
        pieces + (to -> piece) + (rookTo -> pieces(rookFrom)) - from - rookFrom
      case MoveType.CASTLE_QUEENSIDE =>
        val rookFrom = if (piece.side == White) "a1" else "a8"
        val rookTo   = if (piece.side == White) "d1" else "d8"
        pieces + (to -> piece) + (rookTo -> pieces(rookFrom)) - from - rookFrom
      case _ =>
        pieces + (to -> piece) - from
    }

    Board(
      newPieces,
      Some(move.asInstanceOf[Move]),
      newWhiteKingside,
      newWhiteQueenside,
      newBlackKingside,
      newBlackQueenside,
    )

  def canCastleKingside(side: Side): Boolean =
    side match {
      case White => whiteCanCastleKingside
      case Black => blackCanCastleKingside
    }

  def canCastleQueenside(side: Side): Boolean =
    side match {
      case White => whiteCanCastleQueenside
      case Black => blackCanCastleQueenside
    }

  def boardToString(side: Side): String =
    val buf = new StringBuilder("\n")

    // Add file labels at top
    buf.append("    a  b  c  d  e  f  g  h\n")
    buf.append("   ------------------------\n")

    val perspective = if side == White then 7 to 0 by -1 else 0 to 7

    for (rank <- perspective) {
      // Add rank number at start of line
      buf.append(s"${rank + 1} |")

      for (file <- 0 to 7) {
        val piece = getPiece(file, rank)
        val str = piece match {
          case Some(WhitePawn)   => "P"
          case Some(WhiteKnight) => "N"
          case Some(WhiteBishop) => "B"
          case Some(WhiteRook)   => "R"
          case Some(WhiteQueen)  => "Q"
          case Some(WhiteKing)   => "K"
          case Some(BlackPawn)   => "p"
          case Some(BlackKnight) => "n"
          case Some(BlackBishop) => "b"
          case Some(BlackRook)   => "r"
          case Some(BlackQueen)  => "q"
          case Some(BlackKing)   => "k"
          case None              => "."
        }
        buf.append(s" $str ")
      }

      // Add rank number at end of line
      buf.append(s"| ${rank + 1}\n")
    }

    // Add file labels at bottom
    buf.append("   ------------------------\n")
    buf.append("    a  b  c  d  e  f  g  h\n")

    buf.toString
  end boardToString

  override def toString: String = boardToString(White)

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
