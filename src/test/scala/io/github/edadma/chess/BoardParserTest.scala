package io.github.edadma.chess

import org.scalatest.funsuite.AnyFunSuite

class BoardParserTest extends AnyFunSuite {
  test("parseBoardString should correctly parse a board layout") {
    val input = """
     r  n  b  q  k  b  n  r  
     p  p  p  p  p  p  p  p  
     .  .  .  .  .  .  .  .  
     .  .  .  .  .  .  .  .  
     .  .  .  .  .  .  .  .  
     .  .  .  .  .  .  .  .  
     P  P  P  P  P  P  P  P  
     R  N  B  Q  K  B  N  R  
   """

    val expected = Set(
      (0, 0, WhiteRook),
      (1, 0, WhiteKnight),
      (2, 0, WhiteBishop),
      (3, 0, WhiteQueen),
      (4, 0, WhiteKing),
      (5, 0, WhiteBishop),
      (6, 0, WhiteKnight),
      (7, 0, WhiteRook),
      (0, 1, WhitePawn),
      (1, 1, WhitePawn),
      (2, 1, WhitePawn),
      (3, 1, WhitePawn),
      (4, 1, WhitePawn),
      (5, 1, WhitePawn),
      (6, 1, WhitePawn),
      (7, 1, WhitePawn),
      (0, 6, BlackPawn),
      (1, 6, BlackPawn),
      (2, 6, BlackPawn),
      (3, 6, BlackPawn),
      (4, 6, BlackPawn),
      (5, 6, BlackPawn),
      (6, 6, BlackPawn),
      (7, 6, BlackPawn),
      (0, 7, BlackRook),
      (1, 7, BlackKnight),
      (2, 7, BlackBishop),
      (3, 7, BlackQueen),
      (4, 7, BlackKing),
      (5, 7, BlackBishop),
      (6, 7, BlackKnight),
      (7, 7, BlackRook),
    )

    assert(parseBoardString(input) == expected)
  }
}
