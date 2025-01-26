package io.github.edadma.chess

import io.github.edadma.logger.LogLevel.ALL
import pprint.pprintln

@main def run(): Unit =
  val board = Board.fromString(
    """
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  r  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |""".stripMargin,
  )

//  println(board.ray(fromAlgebraic("d4"), -1, 0).toList.map(toAlgebraic))
  println(board.isSquareAttacked(fromAlgebraic("e3"), Black))
