package io.github.edadma.chess

import io.github.edadma.logger.LogLevel.ALL
import pprint.pprintln

@main def run(): Unit =
  val board = Board.fromString(
    """
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  b  .  .  .  .
      |.  .  .  .  P  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |""".stripMargin,
  )

  println(board.isSquareAttacked(fromAlgebraic("g3"), Black))
