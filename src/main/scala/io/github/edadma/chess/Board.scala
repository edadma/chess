package io.github.edadma.chess

object Board {
  // Square constants
  val A1 = 0; val B1  = 1; val C1  = 2; val D1  = 3; val E1  = 4; val F1  = 5; val G1  = 6; val H1  = 7
  val A2 = 8; val B2  = 9; val C2  = 10; val D2 = 11; val E2 = 12; val F2 = 13; val G2 = 14; val H2 = 15
  val A3 = 16; val B3 = 17; val C3 = 18; val D3 = 19; val E3 = 20; val F3 = 21; val G3 = 22; val H3 = 23
  val A4 = 24; val B4 = 25; val C4 = 26; val D4 = 27; val E4 = 28; val F4 = 29; val G4 = 30; val H4 = 31
  val A5 = 32; val B5 = 33; val C5 = 34; val D5 = 35; val E5 = 36; val F5 = 37; val G5 = 38; val H5 = 39
  val A6 = 40; val B6 = 41; val C6 = 42; val D6 = 43; val E6 = 44; val F6 = 45; val G6 = 46; val H6 = 47
  val A7 = 48; val B7 = 49; val C7 = 50; val D7 = 51; val E7 = 52; val F7 = 53; val G7 = 54; val H7 = 55
  val A8 = 56; val B8 = 57; val C8 = 58; val D8 = 59; val E8 = 60; val F8 = 61; val G8 = 62; val H8 = 63

  // Convert algebraic notation (e.g. "e4") to board index (0-63)
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

  // For easier testing - implicit conversion from string to index
  implicit class AlgebraicOps(s: String) {
    def toSquare: Int = fromAlgebraic(s).getOrElse(
      throw new IllegalArgumentException(s"Invalid algebraic notation: $s"),
    )
  }

  // Constants for board representation
  final val EMPTY_BOARD: Long = 0L
  final val FULL_BOARD: Long  = -1L // All bits set to 1
  final val NOT_A_FILE: Long  = 0xfefefefefefefefeL
  final val NOT_H_FILE: Long  = 0x7f7f7f7f7f7f7f7fL

  // Pre-calculated attack tables
  private val KNIGHT_MOVES: Array[Long] = Array.ofDim[Long](64)
  private val KING_MOVES: Array[Long]   = Array.ofDim[Long](64)

  // Initialize attack tables
  {
    val knightDeltas = List(
      (-2, -1),
      (-2, 1), // 2 up, 1 left/right
      (-1, -2),
      (-1, 2), // 1 up, 2 left/right
      (1, -2),
      (1, 2), // 1 down, 2 left/right
      (2, -1),
      (2, 1), // 2 down, 1 left/right
    )

    for (square <- 0 until 64) {
      val row = square / 8
      val col = square % 8

      for {
        (dr, dc) <- knightDeltas
        newRow = row + dr
        newCol = col + dc
        if newRow >= 0 && newRow < 8 && newCol >= 0 && newCol < 8
      } {
        val targetSquare = newRow * 8 + newCol
        KNIGHT_MOVES(square) |= 1L << targetSquare
      }
    }
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

// Enumeration for pieces
sealed trait Piece { def isWhite: Boolean }
case object WhitePawn   extends Piece { def isWhite = true  }
case object WhiteKnight extends Piece { def isWhite = true  }
case object WhiteBishop extends Piece { def isWhite = true  }
case object WhiteRook   extends Piece { def isWhite = true  }
case object WhiteQueen  extends Piece { def isWhite = true  }
case object WhiteKing   extends Piece { def isWhite = true  }
case object BlackPawn   extends Piece { def isWhite = false }
case object BlackKnight extends Piece { def isWhite = false }
case object BlackBishop extends Piece { def isWhite = false }
case object BlackRook   extends Piece { def isWhite = false }
case object BlackQueen  extends Piece { def isWhite = false }
case object BlackKing   extends Piece { def isWhite = false }

case class Board(
    whitePawns: Long = 0xff00L,
    whiteKnights: Long = 0x42L,
    whiteBishops: Long = 0x24L,
    whiteRooks: Long = 0x81L,
    whiteQueens: Long = 0x8L,
    whiteKing: Long = 0x10L,
    blackPawns: Long = 0xff000000000000L,
    blackKnights: Long = 0x4200000000000000L,
    blackBishops: Long = 0x2400000000000000L,
    blackRooks: Long = 0x8100000000000000L,
    blackQueens: Long = 0x800000000000000L,
    blackKing: Long = 0x1000000000000000L,
    castlingRights: Int = 0xf, // KQkq in binary
    enPassantSquare: Option[Int] = None,
    halfMoveClock: Int = 0,
    moveNumber: Int = 1,
    lastMove: Option[Move] = None,
) {
  import Board._

  // Combined occupancy bitboards
  lazy val whitePieces: Long = whitePawns | whiteKnights | whiteBishops | whiteRooks | whiteQueens | whiteKing
  lazy val blackPieces: Long = blackPawns | blackKnights | blackBishops | blackRooks | blackQueens | blackKing
  lazy val occupied: Long    = whitePieces | blackPieces
  lazy val empty: Long       = ~occupied

  private lazy val whiteKingSquare: Int = 63 - java.lang.Long.numberOfLeadingZeros(whiteKing)
  private lazy val blackKingSquare: Int = 63 - java.lang.Long.numberOfLeadingZeros(blackKing)

  // Helper methods for bit manipulation
  private[chess] def getBit(bitboard: Long, square: Int): Boolean = ((bitboard >>> square) & 1L) == 1L
  private[chess] def setBit(bitboard: Long, square: Int): Long    = bitboard | (1L << square)
  private[chess] def clearBit(bitboard: Long, square: Int): Long  = bitboard & ~(1L << square)

  // Get piece at square
  def getPiece(square: Int): Option[Piece] = {
    if (getBit(whitePawns, square)) Some(WhitePawn)
    else if (getBit(whiteKnights, square)) Some(WhiteKnight)
    else if (getBit(whiteBishops, square)) Some(WhiteBishop)
    else if (getBit(whiteRooks, square)) Some(WhiteRook)
    else if (getBit(whiteQueens, square)) Some(WhiteQueen)
    else if (getBit(whiteKing, square)) Some(WhiteKing)
    else if (getBit(blackPawns, square)) Some(BlackPawn)
    else if (getBit(blackKnights, square)) Some(BlackKnight)
    else if (getBit(blackBishops, square)) Some(BlackBishop)
    else if (getBit(blackRooks, square)) Some(BlackRook)
    else if (getBit(blackQueens, square)) Some(BlackQueen)
    else if (getBit(blackKing, square)) Some(BlackKing)
    else None
  }

  // Sliding piece attack generation
//  def getRayAttacks(square: Int, occupied: Long, deltas: Array[(Int, Int)]): Long = {
//    var attacks  = 0L
//    val fromFile = square % 8
//    val fromRank = square / 8
//    logger.debug(s"Getting ray attacks from rank $fromRank, file $fromFile")
//    logger.debug(
//      s"Occupied squares: ${String.format("%64s", java.lang.Long.toBinaryString(occupied)).replace(' ', '0')}",
//    )
//
//    for ((dx, dy) <- deltas) {
//      logger.debug(s"Processing delta: ($dx, $dy)")
//      var x        = fromFile
//      var y        = fromRank
//      var continue = true
//      while (continue) {
//        x += dx
//        y += dy
//        logger.debug(s"Checking x=$x, y=$y")
//        // Check if we've moved off the board OR wrapped around a file
//        if (
//          x < 0 || x > 7 || y < 0 || y > 7 ||
//          (dx > 0 && x < fromFile) || // Wrapped right to left
//          (dx < 0 && x > fromFile)
//        ) { // Wrapped left to right
//          logger.debug("Hit board edge")
//          continue = false
//        } else {
//          val targetSquare = y * 8 + x
//          val isOccupied   = getBit(occupied, targetSquare)
//          logger.debug(s"Square ${toAlgebraic(targetSquare)} (square $targetSquare) occupied? $isOccupied")
//          attacks = setBit(attacks, targetSquare)
//          if (isOccupied) {
//            logger.debug(s"Hit piece at ${toAlgebraic(targetSquare)}")
//            continue = false
//          }
//        }
//      }
//    }
//    attacks
//  }

//  def getRayAttacks(square: Int, occupied: Long, deltas: Array[(Int, Int)]): Long = {
//    var attacks  = 0L
//    val fromFile = square % 8
//    val fromRank = square / 8
//    logger.debug(s"Getting ray attacks from rank $fromRank, file $fromFile (${toAlgebraic(square)})")
//
//    for ((dx, dy) <- deltas) {
//      logger.debug(s"Processing delta: ($dx, $dy)")
//      var x        = fromFile
//      var y        = fromRank
//      var continue = true
//      while (continue) {
//        val nextX = x + dx
//        val nextY = y + dy
//        logger.debug(s"At ($x,$y) moving to ($nextX,$nextY)")
//        if (nextX < 0 || nextX > 7 || nextY < 0 || nextY > 7) {
//          logger.debug("Hit board edge")
//          continue = false
//        } else {
//          val targetSquare = nextY * 8 + nextX
//          logger.debug(s"Moving to square $targetSquare (${toAlgebraic(targetSquare)})")
//          val isOccupied = getBit(occupied, targetSquare)
//          logger.debug(s"Square ${toAlgebraic(targetSquare)} ($targetSquare) occupied? $isOccupied")
//          attacks = setBit(attacks, targetSquare)
//          if (isOccupied) {
//            logger.debug(s"Hit piece at ${toAlgebraic(targetSquare)}")
//            continue = false
//          }
//          x = nextX
//          y = nextY
//        }
//      }
//    }
//    attacks
//  }

  def getRayAttacks(square: Int, occupied: Long, deltas: Array[(Int, Int)]): Long = {
    var attacks  = 0L
    val fromFile = square % 8
    val fromRank = square / 8
    logger.debug(s"Getting ray attacks from rank $fromRank, file $fromFile (${toAlgebraic(square)})")

    for ((dx, dy) <- deltas) {
      logger.debug(s"Processing delta: ($dx, $dy)")
      var x        = fromFile
      var y        = fromRank
      var continue = true
      while (continue) {
        val nextX = x + dx
        val nextY = y + dy
        logger.debug(s"At ($x,$y) moving to ($nextX,$nextY)")
        if (nextX < 0 || nextX > 7 || nextY < 0 || nextY > 7) {
          logger.debug("Hit board edge")
          continue = false
        } else {
          val targetSquare = nextY * 8 + nextX
          logger.debug(s"Moving to square $targetSquare (${toAlgebraic(targetSquare)})")
          val isOccupied = getBit(occupied, targetSquare)
          logger.debug(s"Square ${toAlgebraic(targetSquare)} ($targetSquare) occupied? $isOccupied")

          if (isOccupied) {
            logger.debug(s"Hit piece at ${toAlgebraic(targetSquare)}")
            attacks = setBit(attacks, targetSquare)
            continue = false
          } else {
            attacks = setBit(attacks, targetSquare)
          }
          x = nextX
          y = nextY
        }
      }
    }
    attacks
  }

  // Specific piece attack patterns
  private val BISHOP_DELTAS = Array((-1, -1), (-1, 1), (1, -1), (1, 1))
  private val ROOK_DELTAS   = Array((-1, 0), (1, 0), (0, -1), (0, 1))

  def getBishopAttacks(square: Int): Long = getRayAttacks(square, occupied, BISHOP_DELTAS)

  def getRookAttacks(square: Int): Long = {
    val attacks = getRayAttacks(square, occupied, ROOK_DELTAS)
    logger.debug(s"Calculating rook attacks from square ${toAlgebraic(square)}")
    logger.debug(s"Attack pattern: ${java.lang.Long.toBinaryString(attacks)}")
    attacks
  }

  def getQueenAttacks(square: Int): Long = getBishopAttacks(square) | getRookAttacks(square)

  def generateMoves(side: Side): Iterator[Move] = {
    logger.debug(s"Generating moves for ${side}")
    val forWhite: Boolean = side == White
    val friendlyPieces    = if (forWhite) whitePieces else blackPieces
    val enemyPieces       = if (forWhite) blackPieces else whitePieces
    val promotionPieces = if (forWhite)
      List(WhiteQueen, WhiteRook, WhiteBishop, WhiteKnight)
    else
      List(BlackQueen, BlackRook, BlackBishop, BlackKnight)

    val regularMoves = for {
      fromSquare <- (0 until 64).iterator
      if getBit(friendlyPieces, fromSquare)
      piece = getPiece(fromSquare).get
      possibleMoves = piece match {
        case WhitePawn | BlackPawn =>
          generatePawnMoves(fromSquare, side)
        case WhiteKnight | BlackKnight =>
          KNIGHT_MOVES(fromSquare) & ~friendlyPieces
        case WhiteBishop | BlackBishop =>
          getBishopAttacks(fromSquare) & ~friendlyPieces
        case WhiteRook | BlackRook =>
          getRookAttacks(fromSquare) & ~friendlyPieces
        case WhiteQueen | BlackQueen =>
          getQueenAttacks(fromSquare) & ~friendlyPieces
        case WhiteKing | BlackKing =>
          KING_MOVES(fromSquare) & ~friendlyPieces
      }
      toSquare <- (0 until 64).iterator
      if getBit(possibleMoves, toSquare)
      move <- {
        val isPromotion = (piece == WhitePawn && toSquare / 8 == 7) ||
          (piece == BlackPawn && toSquare / 8 == 0)
        val isEnPassant = enPassantSquare.contains(toSquare)
        val capturedPiece =
          if (isEnPassant) {
            Some(if (side == White) BlackPawn else WhitePawn)
          } else if (getBit(enemyPieces, toSquare)) {
            getPiece(toSquare)
          } else {
            None
          }

        if (isPromotion)
          promotionPieces.iterator.map(promotionPiece =>
            Move(
              fromSquare,
              toSquare,
              piece,
              capture = capturedPiece,
              promotion = Some(promotionPiece),
              isEnPassant = isEnPassant,
            ),
          )
        else
          Iterator.single(Move(
            fromSquare,
            toSquare,
            piece,
            capture = capturedPiece,
            isEnPassant = isEnPassant,
          ))
      }
    } yield move

    val inCheck = isCheck(side)
    logger.debug(s"Side is in check: $inCheck")

    if (inCheck)
      logger.debug("In check - skipping castling moves")
      regularMoves
    else
      logger.debug("Not in check - including castling moves")
      val castlingMoves = generateCastlingMoves(side).toList
      logger.debug(s"Generated castling moves: $castlingMoves")
      regularMoves ++ generateCastlingMoves(side)
  }

  private def generateCastlingMoves(side: Side): Iterator[Move] = {
    val forWhite: Boolean = side == White
    val rank              = if (forWhite) 0 else 7
    val kingSquare        = rank * 8 + 4

    logger.debug(s"Generating castling moves for $side")
    logger.debug(s"Looking for king at square $kingSquare")

    getPiece(kingSquare) match {
      case Some(king) =>
        logger.debug(s"Found king: $king")
        val kingsidePossible = canCastleKingside(side)
        logger.debug(s"Kingside castle possible: $kingsidePossible")

        (if (kingsidePossible)
           Iterator.single(Move(kingSquare, kingSquare + 2, king, isCastling = true))
         else Iterator.empty) ++
          (if (canCastleQueenside(side))
             Iterator.single(Move(kingSquare, kingSquare - 2, king, isCastling = true))
           else Iterator.empty)
      case None =>
        logger.debug(s"No king found at square $kingSquare")
        Iterator.empty
    }
  }

  private def generatePawnMoves(square: Int, side: Side): Long = {
    val whiteToMove = side == White
    var moves       = 0L
    val (singlePush, doublePush, leftCapture, rightCapture) =
      if (whiteToMove) (8, 16, 7, 9) else (-8, -16, -9, -7)

    // Get enemy pieces for captures
    val enemyPieces = if (whiteToMove) blackPieces else whitePieces

    // Check if pawn is on promotion rank
    val onPromotionRank = (whiteToMove && square / 8 == 6) || (!whiteToMove && square / 8 == 1)

    // Single push (only to empty squares)
    if (!getBit(occupied, square + singlePush)) {
      moves = setBit(moves, square + singlePush)

      // Double push from starting rank
      if (
        !onPromotionRank &&
        ((whiteToMove && square / 8 == 1) || (!whiteToMove && square / 8 == 6)) &&
        !getBit(occupied, square + doublePush)
      ) {
        moves = setBit(moves, square + doublePush)
      }
    }

    // Captures and promotions
    if (square % 8 > 0 && getBit(enemyPieces, square + leftCapture)) {
      moves = setBit(moves, square + leftCapture)
    }
    if (square % 8 < 7 && getBit(enemyPieces, square + rightCapture)) {
      moves = setBit(moves, square + rightCapture)
    }

    // En passant captures
    enPassantSquare.foreach { epSquare =>
      lastMove.foreach { move =>
        if (
          (move.piece == WhitePawn && !whiteToMove) ||
          (move.piece == BlackPawn && whiteToMove)
        ) {
          if (math.abs(move.to - move.from) == 16) {
            if (
              square % 8 > 0 && epSquare == square + leftCapture ||
              square % 8 < 7 && epSquare == square + rightCapture
            ) {
              moves = setBit(moves, epSquare)
            }
          }
        }
      }
    }

    moves
  }

  def isCheck(side: Side): Boolean =
    val kingSquare = if (side == White) whiteKingSquare else blackKingSquare

    kingSquare != -1 && isSquareAttacked(kingSquare, side)

  def makeMove(move: Move): Board = {
    def updateBitboard(bb: Long, from: Int, to: Int): Long = {
      if (getBit(bb, from)) setBit(clearBit(bb, from), to) else bb
    }

    val newEnPassantSquare = {
      if (
        (move.piece == WhitePawn || move.piece == BlackPawn) &&
        math.abs(move.to - move.from) == 16
      ) {
        Some((move.from + move.to) / 2)
      } else None
    }

    copy(
      whitePawns = updateBitboard(whitePawns, move.from, move.to),
      whiteKnights = updateBitboard(whiteKnights, move.from, move.to),
      whiteBishops = updateBitboard(whiteBishops, move.from, move.to),
      whiteRooks = updateBitboard(whiteRooks, move.from, move.to),
      whiteQueens = updateBitboard(whiteQueens, move.from, move.to),
      whiteKing = updateBitboard(whiteKing, move.from, move.to),
      blackPawns = updateBitboard(blackPawns, move.from, move.to),
      blackKnights = updateBitboard(blackKnights, move.from, move.to),
      blackBishops = updateBitboard(blackBishops, move.from, move.to),
      blackRooks = updateBitboard(blackRooks, move.from, move.to),
      blackQueens = updateBitboard(blackQueens, move.from, move.to),
      blackKing = updateBitboard(blackKing, move.from, move.to),
      enPassantSquare = newEnPassantSquare,
      lastMove = Some(move),
    )
  }

  def isSquareAttacked(square: Int, side: Side): Boolean = {
    val byWhite      = side == White
    val enemyPawns   = if (byWhite) blackPawns else whitePawns
    val enemyKnights = if (byWhite) blackKnights else whiteKnights
    val enemyBishops = if (byWhite) blackBishops else whiteBishops
    val enemyRooks   = if (byWhite) blackRooks else whiteRooks
    val enemyQueens  = if (byWhite) blackQueens else whiteQueens
    val enemyKing    = if (byWhite) blackKing else whiteKing

    logger.debug(s"Checking if square ${toAlgebraic(square)} is attacked by $side")

    // Pawn attacks
    val pawnAttacks = if (byWhite) {
      // If we're checking white being attacked, look at black pawn attacks going down
      ((enemyPawns & NOT_H_FILE) >>> 9) | ((enemyPawns & NOT_A_FILE) >>> 7)
    } else {
      // If we're checking black being attacked, look at white pawn attacks going up
      ((enemyPawns & NOT_H_FILE) << 7) | ((enemyPawns & NOT_A_FILE) << 9)
    }
    val underPawnAttack = getBit(pawnAttacks, square)
    logger.debug(s"Under pawn attack: $underPawnAttack")
    if (underPawnAttack) return true

    // Knight attacks
    val underKnightAttack = (KNIGHT_MOVES(square) & enemyKnights) != 0
    logger.debug(s"Under knight attack: $underKnightAttack")
    if (underKnightAttack) return true

    // Bishop/Queen diagonal attacks
    val fromSquare = square
    val fromFile   = fromSquare % 8
    val fromRank   = fromSquare / 8

    // Check each diagonal direction
    for {
      (dx, dy) <- BISHOP_DELTAS
      x = Iterator.iterate(fromFile)(_ + dx).takeWhile(x => x >= 0 && x <= 7)
      y = Iterator.iterate(fromRank)(_ + dy).takeWhile(y => y >= 0 && y <= 7)
      (currX, currY) <- x.zip(y)
    } {
      val targetSquare = currY * 8 + currX
      if (getBit(enemyBishops | enemyQueens, targetSquare)) return true
      if (getBit(occupied, targetSquare)) return false
    }

    // Rook/Queen straight attacks
    for {
      (dx, dy) <- ROOK_DELTAS
      x = Iterator.iterate(fromFile)(_ + dx).takeWhile(x => x >= 0 && x <= 7)
      y = Iterator.iterate(fromRank)(_ + dy).takeWhile(y => y >= 0 && y <= 7)
      (currX, currY) <- x.zip(y)
    } {
      val targetSquare = currY * 8 + currX
      if (getBit(enemyRooks | enemyQueens, targetSquare)) return true
      if (getBit(occupied, targetSquare)) return false
    }

    // King attacks
    val underKingAttack = (KING_MOVES(square) & enemyKing) != 0
    logger.debug(s"Under king attack: $underKingAttack")

    underKingAttack
  }

//  def isSquareAttacked(square: Int, side: Side): Boolean = {
//    val byWhite      = side == White
//    val enemyPawns   = if (byWhite) blackPawns else whitePawns
//    val enemyKnights = if (byWhite) blackKnights else whiteKnights
//    val enemyBishops = if (byWhite) blackBishops else whiteBishops
//    val enemyRooks   = if (byWhite) blackRooks else whiteRooks
//    val enemyQueens  = if (byWhite) blackQueens else whiteQueens
//    val enemyKing    = if (byWhite) blackKing else whiteKing
//
//    logger.debug(s"Checking if square ${toAlgebraic(square)} is attacked by $side")
//
//    // Pawn attacks
//    val pawnAttacks = if (byWhite) {
//      // If we're checking white being attacked, look at black pawn attacks going down
//      ((enemyPawns & NOT_H_FILE) >>> 9) | ((enemyPawns & NOT_A_FILE) >>> 7)
//    } else {
//      // If we're checking black being attacked, look at white pawn attacks going up
//      ((enemyPawns & NOT_H_FILE) << 7) | ((enemyPawns & NOT_A_FILE) << 9)
//    }
//    val underPawnAttack = getBit(pawnAttacks, square)
//    logger.debug(s"Under pawn attack: $underPawnAttack")
//    if (underPawnAttack) return true
//
//    // Knight attacks
//    val underKnightAttack = (KNIGHT_MOVES(square) & enemyKnights) != 0
//    logger.debug(s"Under knight attack: $underKnightAttack")
//    if (underKnightAttack) return true
//
//    // Bishop/Queen diagonal attacks
//    val bishopAttacks = getBishopAttacks(square)
//    if ((bishopAttacks & (enemyBishops | enemyQueens)) != 0) return true
//
//    // Rook/Queen straight attacks
//    val rookAttacks = getRookAttacks(square)
//    if ((rookAttacks & (enemyRooks | enemyQueens)) != 0) return true
//
//    // King attacks
//    val underKingAttack = (KING_MOVES(square) & enemyKing) != 0
//    logger.debug(s"Under king attack: $underKingAttack")
//
//    underKingAttack
//  }

  def isStalemate(side: Side): Boolean = {
    // Not stalemate if in check
    if (isCheck(side)) return false

    // Stalemate if no legal moves and not in check
    !hasLegalMoves(side)
  }

  def hasLegalMoves(side: Side): Boolean = generateLegalMoves(side).nonEmpty

  def hasInsufficientMaterial: Boolean = {
    // King vs King
    if (occupied == (whiteKing | blackKing)) return true

    // King and minor piece vs King
    val whiteMaterial = whitePieces & ~whiteKing
    val blackMaterial = blackPieces & ~blackKing

    if (java.lang.Long.bitCount(whiteMaterial | blackMaterial) <= 1) {
      val onlyMinors = (whiteKnights | whiteBishops | blackKnights | blackBishops)
      return (whiteMaterial | blackMaterial) == onlyMinors
    }

    false
  }

  def generateLegalMoves(side: Side): Iterator[Move] = {
    generateMoves(side).filter(move => {
      val newBoard = makeMove(move)

      !newBoard.isCheck(side)
    })
  }

//  def canCastleKingside(side: Side): Boolean = {
//    val rank   = if (side == White) 0 else 7
//    val rights = if (side == White) castlingRights & 0x1 else castlingRights & 0x4
//    logger.debug(s"Checking kingside castle for $side")
//    logger.debug(s"Castling rights check: ${rights != 0}")
//
//    if (rights == 0) return false
//
//    val squares      = Array(rank * 8 + 5, rank * 8 + 6) // F1,G1 for white
//    val squaresEmpty = !squares.exists(sq => getBit(occupied, sq))
//    logger.debug(s"Intermediate squares empty: $squaresEmpty")
//
//    if (!squaresEmpty) return false
//
//    val squaresNotAttacked = squares.forall(sq => !isSquareAttacked(sq, side.opposite))
//    logger.debug(s"Squares not under attack: $squaresNotAttacked")
//
//    squaresNotAttacked
//  }

//  def canCastleKingside(side: Side): Boolean = {
//    val rank   = if (side == White) 0 else 7
//    val rights = if (side == White) castlingRights & 0x1 else castlingRights & 0x4
//    logger.debug(s"Checking kingside castle for $side")
//    logger.debug(s"Castling rights check: ${rights != 0}")
//
//    if (rights == 0) return false
//
//    val squares      = Array(rank * 8 + 5, rank * 8 + 6) // F1,G1 for white
//    val squaresEmpty = !squares.exists(sq => getBit(occupied, sq))
//    logger.debug(s"Intermediate squares empty: $squaresEmpty")
//
//    if (!squaresEmpty) return false
//
//    val squaresNotAttacked = squares.forall(sq => !isSquareAttacked(sq, side.opposite))
//    logger.debug(s"Squares not under attack: $squaresNotAttacked")
//
//    squaresNotAttacked
//  }

  def canCastleKingside(side: Side): Boolean = {
    val rank   = if (side == White) 0 else 7
    val rights = if (side == White) castlingRights & 0x1 else castlingRights & 0x4
    logger.debug(s"Checking kingside castle for $side")
    logger.debug(s"Castling rights check: ${rights != 0}")

    if (rights == 0) return false

    val squares = Array(rank * 8 + 5, rank * 8 + 6)
    logger.debug(s"Checking squares: ${squares.map(Board.toAlgebraic).mkString(", ")}") // Add this line

    val squaresEmpty = !squares.exists(sq => getBit(occupied, sq))
    logger.debug(s"Intermediate squares empty: $squaresEmpty")

    if (!squaresEmpty) return false

    val squaresNotAttacked = squares.forall(sq => !isSquareAttacked(sq, side))
    logger.debug(s"Squares not under attack: $squaresNotAttacked")

    squaresNotAttacked
  }

  def canCastleQueenside(side: Side): Boolean = {
    val rank   = if (side == White) 0 else 7
    val rights = if (side == White) castlingRights & 0x2 else castlingRights & 0x8
    if (rights == 0) return false

    // Check squares between king and rook are empty
    val squares = Array(rank * 8 + 3, rank * 8 + 2, rank * 8 + 1)
    if (squares.exists(sq => getBit(occupied, sq))) return false

    // Verify squares king moves through aren't attacked
    squares.take(2).forall(sq => !isSquareAttacked(sq, side))
  }
}
