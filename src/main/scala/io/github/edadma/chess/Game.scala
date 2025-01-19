package io.github.edadma.chess

import io.github.edadma.logger.LoggerFactory

case class GameState(
    board: Board,
    sideToMove: Side,
    moveNumber: Int,
    lastMove: Option[Move],
)

class Game {
  private var states = List(GameState(Board(), White, 1, None))
  private val logger = LoggerFactory.getLogger

  def getCurrentBoard: Board      = states.head.board
  def getCurrentTurn: Side        = states.head.sideToMove
  def getMoveNumber: Int          = states.head.moveNumber
  def getHistory: List[GameState] = states
  def getLastMove: Option[Move]   = states.head.lastMove

  def makeMove(move: Move): Boolean = {
    val board = getCurrentBoard

    if (!board.generateLegalMoves(getCurrentTurn).exists(_ == move)) {
      logger.warn(s"Illegal move attempted: $move")
      return false
    }

    // Update board with move
    val newBoard = applyMove(board, move)

    // Add new state
    states = GameState(
      newBoard,
      getCurrentTurn.opposite,
      if (getCurrentTurn == Black) getMoveNumber + 1 else getMoveNumber,
      Some(move),
    ) :: states

    logger.info(s"Move made: $move")
    true
  }

  def undoMove(): Boolean = {
    if (states.length == 1) {
      logger.warn("Cannot undo - at initial position")
      false
    } else {
      states = states.tail
      logger.info("Move undone")
      true
    }
  }

  def isCheckmate(side: Side): Boolean =
    getCurrentBoard.isCheck(side) && !getCurrentBoard.hasLegalMoves(side)

  def isStalemate(side: Side): Boolean = getCurrentBoard.isStalemate(side)

  def isDraw: Boolean =
    isStalemate(getCurrentTurn) ||
      getCurrentBoard.hasInsufficientMaterial ||
      isThreefoldRepetition

  private def isThreefoldRepetition: Boolean = {
    val currentBoard = getCurrentBoard
    states.count(_.board == currentBoard) >= 3
  }

  private def applyMove(board: Board, move: Move): Board = {
    // Handle castling
    if (move.isCastling) {
      val rank = if (move.piece.isWhite) 0 else 7
      val (rookFrom, rookTo) =
        if (move.to % 8 == 6) (rank * 8 + 7, rank * 8 + 5) // Kingside
        else (rank * 8 + 0, rank * 8 + 3) // Queenside

      board.makeMove(move).makeMove(Move(rookFrom, rookTo, if (move.piece.isWhite) WhiteRook else BlackRook))
    } else board.makeMove(move)
  }
}
