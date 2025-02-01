package io.github.edadma.chess

class BasicEvaluator(rules: List[Evaluator]) extends Evaluator {
  def evaluate(position: ChessBoard, side: Side): Int = {
    if (position.isCheckmate(side)) Int.MinValue
    else if (position.isCheckmate(side.opposite)) Int.MaxValue
    else rules.map(rule => rule.evaluate(position, side)).sum
  }
}
