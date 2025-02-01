package io.github.edadma.chess

trait Engine:
  def makeMove(game: Game): Option[ChessMove] = findBestMove(game.getBoard, evaluator)
  def evaluator: Evaluator
  def findBestMove(position: ChessBoard, evaluator: Evaluator): Option[ChessMove]

trait Evaluator {
  // Evaluate a position from the perspective of the side to move
  def evaluate(position: ChessBoard, side: Side): Int
}
