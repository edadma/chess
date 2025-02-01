package io.github.edadma.chess

class MinimaxEngine(depth: Int) extends Engine {
  def findBestMove(position: ChessBoard, evaluator: Evaluator): Option[ChessMove] = {
    def minimax(pos: ChessBoard, depth: Int, side: Side): Int = {
      if (depth == 0) return evaluator.evaluate(pos, side)

      val moves = pos.getMoves(side)
      if (moves.isEmpty) return evaluator.evaluate(pos, side)

      val scores = moves.map { move =>
        -minimax(pos.applyMove(move), depth - 1, side.opposite)
      }

      if (side == position.getCurrentTurn) scores.max else scores.min
    }

    position.getMoves(position.getCurrentTurn).maxByOption { move =>
      -minimax(position.applyMove(move), depth - 1, position.getCurrentTurn.opposite)
    }
  }
}
