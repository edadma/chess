package io.github.edadma.chess

import io.github.edadma.logger.LogLevel.ALL
import pprint.pprintln

@main def run(): Unit =
  val board = Board.fromString("""
                                 |.  .  .  r  .  .  .  .
                                 |.  .  .  .  .  .  .  .
                                 |.  .  .  Q  .  .  .  .
                                 |.  .  .  K  .  .  .  .
                                 |.  .  .  .  .  .  .  .
                                 |.  .  .  .  .  .  .  .
                                 |.  .  .  .  .  .  .  .
                                 |.  .  .  .  .  .  .  .""".stripMargin.trim)

  logger.setLogLevel(ALL)

  val moves = board.generateMoves(White).filter(_.piece == WhiteQueen).map(_.to).toSet

  pprintln(moves)
