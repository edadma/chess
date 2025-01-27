package io.github.edadma.chess

import io.github.edadma.logger.LogLevel.ALL
import pprint.pprintln

@main def run(): Unit =
  val board = Board.fromString(
    """
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  k  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  p  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  .  .
      |""".stripMargin,
  )

  println(board.getMoves(Black).filter(_.piece == BlackKnight).toList)
