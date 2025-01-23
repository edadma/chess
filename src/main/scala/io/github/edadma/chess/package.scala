package io.github.edadma.chess

import io.github.edadma.logger.LoggerFactory

val logger = LoggerFactory.newLogger

def fromAlgebraic(s: String): Int = {
  require(s.length == 2, "SAN has two characters")

  val file = s(0).toLower - 'a'
  val rank = s(1).asDigit - 1

  require(file >= 0 && file <= 7 && rank >= 0 && rank <= 7, "SAN is a letter (a-h) and a digit (0-7)")
  rank * 8 + file
}

def toAlgebraic(square: Int): String = {
  require(square >= 0 && square < 64, "Square index must be between 0 and 63")

  val file = ('a' + square % 8).toChar
  val rank = (square / 8 + 1).toString
  file.toString + rank
}
