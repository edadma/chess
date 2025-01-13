package io.github.edadma.chess

sealed trait Color

case object White extends Color

case object Black extends Color

sealed trait PieceType

case object Pawn extends PieceType

case object Knight extends PieceType

case object Bishop extends PieceType

case object Rook extends PieceType

case object Queen extends PieceType

case object King extends PieceType

case class Piece(pieceType: PieceType, color: Color)

case class Square(file: Char, rank: Int) {
  def toAlgebraic: String = s"$file$rank"

  def isValid: Boolean =
    file >= 'a' && file <= 'h' && rank >= 1 && rank <= 8

  def offset(fileOffset: Int, rankOffset: Int): Option[Square] = {
    val newFile   = (file.toInt + fileOffset).toChar
    val newRank   = rank + rankOffset
    val newSquare = Square(newFile, newRank)
    if (newSquare.isValid) Some(newSquare) else None
  }
}

object Square {
  def fromAlgebraic(s: String): Option[Square] = {
    if (
      s.length == 2 &&
      s(0).isLetter &&
      s(0) >= 'a' && s(0) <= 'h' &&
      s(1).isDigit &&
      s(1) >= '1' && s(1) <= '8'
    ) {
      Some(Square(s(0), s(1).asDigit))
    } else None
  }
}

case class Move(from: Square, to: Square) {
  def toAlgebraic: String = s"${from.toAlgebraic}${to.toAlgebraic}"
}

class GameState {
  private var pieces: Map[Square, Piece] = Map()
  private var currentTurn: Color         = White

  def initialize(): Unit = {
    // Set up initial chess position
    val backRow = List(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)

    // Place pieces
    for (file <- 'a' to 'h') {
      // Pawns
      pieces += (Square(file, 2) -> Piece(Pawn, White))
      pieces += (Square(file, 7) -> Piece(Pawn, Black))

      // Back rows
      val fileIndex = file - 'a'
      pieces += (Square(file, 1) -> Piece(backRow(fileIndex), White))
      pieces += (Square(file, 8) -> Piece(backRow(fileIndex), Black))
    }
  }

  def getPiece(square: Square): Option[Piece] = pieces.get(square)
  def getCurrentTurn: Color                   = currentTurn

  def makeMove(move: Move): Boolean = {
    if (!isLegalMove(move)) return false

    // Execute move
    getPiece(move.from).foreach { piece =>
      pieces -= move.from
      pieces += (move.to -> piece)
    }

    currentTurn = if (currentTurn == White) Black else White
    true
  }

  private def isLegalMove(move: Move): Boolean = {
    getPiece(move.from) match {
      case None                                      => false
      case Some(piece) if piece.color != currentTurn => false
      case Some(piece) =>
        getLegalMovesForPiece(move.from, piece).contains(move)
    }
  }

  def getLegalMovesForPiece(square: Square, piece: Piece): List[Move] = {
    piece.pieceType match {
      case Pawn   => getPawnMoves(square, piece.color)
      case Knight => getKnightMoves(square, piece.color)
      case Bishop => getBishopMoves(square, piece.color)
      case Rook   => getRookMoves(square, piece.color)
      case Queen  => getQueenMoves(square, piece.color)
      case King   => getKingMoves(square, piece.color)
    }
  }

  private def getPawnMoves(square: Square, color: Color): List[Move] = {
    val direction = if (color == White) 1 else -1
    val moves     = List.newBuilder[Move]

    // Forward one square
    square.offset(0, direction).foreach { target =>
      if (getPiece(target).isEmpty) {
        moves += Move(square, target)

        // Initial two square advance
        if (
          (color == White && square.rank == 2) ||
          (color == Black && square.rank == 7)
        ) {
          square.offset(0, direction * 2).foreach { doubleTarget =>
            if (getPiece(doubleTarget).isEmpty) {
              moves += Move(square, doubleTarget)
            }
          }
        }
      }
    }

    // Captures
    for (fileOffset <- List(-1, 1)) {
      square.offset(fileOffset, direction).foreach { target =>
        getPiece(target).foreach { targetPiece =>
          if (targetPiece.color != color) {
            moves += Move(square, target)
          }
        }
      }
    }

    moves.result()
  }

  private def getKnightMoves(square: Square, color: Color): List[Move] = {
    val offsets = List(
      (-2, -1),
      (-2, 1),
      (-1, -2),
      (-1, 2),
      (1, -2),
      (1, 2),
      (2, -1),
      (2, 1),
    )

    offsets.flatMap { case (fileOffset, rankOffset) =>
      square.offset(fileOffset, rankOffset).flatMap { target =>
        getPiece(target) match {
          case None                                => Some(Move(square, target))
          case Some(piece) if piece.color != color => Some(Move(square, target))
          case _                                   => None
        }
      }
    }
  }

  private def getStraightMoves(square: Square, color: Color, directions: List[(Int, Int)]): List[Move] = {
    val moves = List.newBuilder[Move]

    for ((fileOffset, rankOffset) <- directions) {
      var currentOffset = 1
      var continue      = true

      while (continue) {
        square.offset(fileOffset * currentOffset, rankOffset * currentOffset) match {
          case None => continue = false
          case Some(target) =>
            getPiece(target) match {
              case None =>
                moves += Move(square, target)
                currentOffset += 1
              case Some(piece) if piece.color != color =>
                moves += Move(square, target)
                continue = false
              case _ => continue = false
            }
        }
      }
    }

    moves.result()
  }

  private def getRookMoves(square: Square, color: Color): List[Move] = {
    val directions = List(
      (0, 1),
      (0, -1),
      (1, 0),
      (-1, 0),
    )
    getStraightMoves(square, color, directions)
  }

  private def getBishopMoves(square: Square, color: Color): List[Move] = {
    val directions = List(
      (1, 1),
      (1, -1),
      (-1, 1),
      (-1, -1),
    )
    getStraightMoves(square, color, directions)
  }

  private def getQueenMoves(square: Square, color: Color): List[Move] = {
    getRookMoves(square, color) ++ getBishopMoves(square, color)
  }

  private def getKingMoves(square: Square, color: Color): List[Move] = {
    val directions = List(
      (0, 1),
      (0, -1),
      (1, 0),
      (-1, 0),
      (1, 1),
      (1, -1),
      (-1, 1),
      (-1, -1),
    )

    directions.flatMap { case (fileOffset, rankOffset) =>
      square.offset(fileOffset, rankOffset).flatMap { target =>
        getPiece(target) match {
          case None                                => Some(Move(square, target))
          case Some(piece) if piece.color != color => Some(Move(square, target))
          case _                                   => None
        }
      }
    }
  }

  def getAllLegalMoves: List[Move] = {
    val moves = for {
      (square, piece) <- pieces
      if piece.color == currentTurn
      move <- getLegalMovesForPiece(square, piece)
    } yield move

    moves.toList
  }
}
