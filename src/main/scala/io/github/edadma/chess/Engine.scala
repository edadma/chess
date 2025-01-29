package io.github.edadma.chess

trait Engine:
  def makeMove(game: Game): Option[ChessMove]
