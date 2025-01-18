package io.github.edadma.chess

object Board {
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
    for (square <- 0 until 64) {
      val row = square / 8
      val col = square % 8

      // Knight moves
      for {
        dr <- Array(-2, -2, -1, -1, 1, 1, 2, 2)
        dc <- Array(-1, 1, -2, 2, -2, 2, -1, 1)
        newRow = row + dr
        newCol = col + dc
        if newRow >= 0 && newRow < 8 && newCol >= 0 && newCol < 8
      } {
        KNIGHT_MOVES(square) |= 1L << (newRow * 8 + newCol)
      }

      // King moves
      for {
        dr <- -1 to 1
        dc <- -1 to 1
        if dr != 0 || dc != 0
        newRow = row + dr
        newCol = col + dc
        if newRow >= 0 && newRow < 8 && newCol >= 0 && newCol < 8
      } {
        KING_MOVES(square) |= 1L << (newRow * 8 + newCol)
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
        case 'p' => blackPawns |= bit
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
      whiteToMove = whiteToMove,
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
    whiteToMove: Boolean = true,
    castlingRights: Int = 0xf, // KQkq in binary
    enPassantSquare: Option[Int] = None,
    halfMoveClock: Int = 0,
    moveNumber: Int = 1,
) {
  import Board._

  // Combined occupancy bitboards
  lazy val whitePieces: Long = whitePawns | whiteKnights | whiteBishops | whiteRooks | whiteQueens | whiteKing
  lazy val blackPieces: Long = blackPawns | blackKnights | blackBishops | blackRooks | blackQueens | blackKing
  lazy val occupied: Long    = whitePieces | blackPieces
  lazy val empty: Long       = ~occupied

  // Helper methods for bit manipulation
  private def getBit(bitboard: Long, square: Int): Boolean = ((bitboard >>> square) & 1L) == 1L
  private def setBit(bitboard: Long, square: Int): Long    = bitboard | (1L << square)
  private def clearBit(bitboard: Long, square: Int): Long  = bitboard & ~(1L << square)

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
  private def getRayAttacks(square: Int, occupied: Long, deltas: Array[(Int, Int)]): Long = {
    var attacks = 0L
    for ((dx, dy) <- deltas) {
      var x        = square % 8
      var y        = square / 8
      var continue = true
      while (continue) {
        x += dx
        y += dy
        if (x < 0 || x > 7 || y < 0 || y > 7) {
          continue = false // Off board
        } else {
          val targetSquare = y * 8 + x
          attacks = setBit(attacks, targetSquare)
          if (getBit(occupied, targetSquare)) {
            continue = false // Blocking piece
          }
        }
      }
    }
    attacks
  }

  // Specific piece attack patterns
  private val BISHOP_DELTAS = Array((-1, -1), (-1, 1), (1, -1), (1, 1))
  private val ROOK_DELTAS   = Array((-1, 0), (1, 0), (0, -1), (0, 1))

  private def getBishopAttacks(square: Int): Long =
    getRayAttacks(square, occupied, BISHOP_DELTAS)

  private def getRookAttacks(square: Int): Long =
    getRayAttacks(square, occupied, ROOK_DELTAS)

  private def getQueenAttacks(square: Int): Long =
    getBishopAttacks(square) | getRookAttacks(square)

  // Move generation
  def generateMoves: Iterator[Move] = {
    val friendlyPieces = if (whiteToMove) whitePieces else blackPieces
    val enemyPieces    = if (whiteToMove) blackPieces else whitePieces

    // Generate all possible moves
    (for {
      fromSquare <- 0 until 64
      if getBit(friendlyPieces, fromSquare)
      piece = getPiece(fromSquare).get
      attacks = piece match {
        case WhitePawn | BlackPawn     => generatePawnMoves(fromSquare)
        case WhiteKnight | BlackKnight => KNIGHT_MOVES(fromSquare)
        case WhiteBishop | BlackBishop => getBishopAttacks(fromSquare)
        case WhiteRook | BlackRook     => getRookAttacks(fromSquare)
        case WhiteQueen | BlackQueen   => getQueenAttacks(fromSquare)
        case WhiteKing | BlackKing     => KING_MOVES(fromSquare)
      }
      toSquare <- 0 until 64
      if getBit(attacks, toSquare) && !getBit(friendlyPieces, toSquare)
      // Additional validation (check, pins, etc.) would go here
    } yield {
      Move(
        from = fromSquare,
        to = toSquare,
        piece = piece,
        capture = if (getBit(enemyPieces, toSquare)) getPiece(toSquare) else None,
      )
    }).iterator
  }

  private def generatePawnMoves(square: Int): Long = {
    var moves = 0L
    val (singlePush, doublePush, leftCapture, rightCapture) =
      if (whiteToMove) {
        (8, 16, 7, 9)
      } else {
        (-8, -16, -9, -7)
      }

    // Single push
    if (!getBit(occupied, square + singlePush)) {
      moves = setBit(moves, square + singlePush)
      // Double push
      if ((whiteToMove && square / 8 == 1) || (!whiteToMove && square / 8 == 6)) {
        if (!getBit(occupied, square + doublePush)) {
          moves = setBit(moves, square + doublePush)
        }
      }
    }

    // Captures
    val enemyPieces = if (whiteToMove) blackPieces else whitePieces
    if (square % 8 > 0) { // Not on A-file
      if (getBit(enemyPieces, square + leftCapture)) {
        moves = setBit(moves, square + leftCapture)
      }
    }
    if (square % 8 < 7) { // Not on H-file
      if (getBit(enemyPieces, square + rightCapture)) {
        moves = setBit(moves, square + rightCapture)
      }
    }

    // En passant
    enPassantSquare.foreach { epSquare =>
      if (
        (square % 8 > 0 && epSquare == square + leftCapture) ||
        (square % 8 < 7 && epSquare == square + rightCapture)
      ) {
        moves = setBit(moves, epSquare)
      }
    }

    moves
  }

  // Check detection
  def isInCheck(whiteKing: Boolean): Boolean = {
    val kingSquare = if (whiteKing) {
      63 - java.lang.Long.numberOfLeadingZeros(this.whiteKing)
    } else {
      63 - java.lang.Long.numberOfLeadingZeros(this.blackKing)
    }

    isSquareAttacked(kingSquare, whiteKing)
  }

  def isCheckmate(whiteKing: Boolean): Boolean = {
    // If not in check, it's not checkmate
    if (!isInCheck(whiteKing)) return false

    // Generate all possible moves
    val moves = generateMoves

    // Try each move to see if it gets us out of check
    while (moves.hasNext) {
      val move     = moves.next()
      val newBoard = makeTestMove(move)
      if (!newBoard.isInCheck(whiteKing)) {
        return false // Found a legal move that escapes check
      }
    }

    true // No legal moves found to escape check
  }

  // Like makeMove but only updates piece positions - for check testing
  private def makeTestMove(move: Move): Board = {
    def updateBitboard(bb: Long, from: Int, to: Int): Long = {
      if (getBit(bb, from)) setBit(clearBit(bb, from), to) else bb
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
    )
  }

  private def isSquareAttacked(square: Int, byWhite: Boolean): Boolean = {
    val enemyPawns   = if (byWhite) blackPawns else whitePawns
    val enemyKnights = if (byWhite) blackKnights else whiteKnights
    val enemyBishops = if (byWhite) blackBishops else whiteBishops
    val enemyRooks   = if (byWhite) blackRooks else whiteRooks
    val enemyQueens  = if (byWhite) blackQueens else whiteQueens
    val enemyKing    = if (byWhite) blackKing else whiteKing

    // Pawn attacks
    val pawnAttacks = if (byWhite) {
      ((enemyPawns & NOT_A_FILE) << 9) | ((enemyPawns & NOT_H_FILE) << 7)
    } else {
      ((enemyPawns & NOT_A_FILE) >>> 7) | ((enemyPawns & NOT_H_FILE) >>> 9)
    }

    if (getBit(pawnAttacks, square)) return true

    // Knight attacks
    if (getBit(KNIGHT_MOVES(square) & enemyKnights, square)) return true

    // Bishop/Queen attacks
    val bishopAttacks = getBishopAttacks(square)
    if (getBit(bishopAttacks & (enemyBishops | enemyQueens), square)) return true

    // Rook/Queen attacks
    val rookAttacks = getRookAttacks(square)
    if (getBit(rookAttacks & (enemyRooks | enemyQueens), square)) return true

    // King attacks
    getBit(KING_MOVES(square) & enemyKing, square)
  }
}
