package io.github.edadma.chess

import io.github.edadma.logger.LogLevel.ALL
import pprint.pprintln

@main def run(): Unit =
  val board = Board.fromString(
    """
      |r  n  b  .  k  b  n  r
      |p  p  p  p  .  p  p  p
      |.  .  .  .  p  .  .  .
      |.  .  .  .  .  .  .  .
      |.  .  .  .  .  .  P  q
      |.  .  .  .  .  P  .  .
      |P  P  P  P  P  .  .  P
      |R  N  B  Q  K  B  N  R
      |""".stripMargin,
  )

  println(board)
