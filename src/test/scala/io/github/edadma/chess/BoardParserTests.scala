package io.github.edadma.chess

import org.scalatest.funsuite.AnyFunSuite

class BoardParserTests extends AnyFunSuite {
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
      (0, WhiteRook),
      (1, WhiteKnight),
      (2, WhiteBishop),
      (3, WhiteQueen),
      (4, WhiteKing),
      (5, WhiteBishop),
      (6, WhiteKnight),
      (7, WhiteRook),
      (8, WhitePawn),
      (9, WhitePawn),
      (10, WhitePawn),
      (11, WhitePawn),
      (12, WhitePawn),
      (13, WhitePawn),
      (14, WhitePawn),
      (15, WhitePawn),
      (48, BlackPawn),
      (49, BlackPawn),
      (50, BlackPawn),
      (51, BlackPawn),
      (52, BlackPawn),
      (53, BlackPawn),
      (54, BlackPawn),
      (55, BlackPawn),
      (56, BlackRook),
      (57, BlackKnight),
      (58, BlackBishop),
      (59, BlackQueen),
      (60, BlackKing),
      (61, BlackBishop),
      (62, BlackKnight),
      (63, BlackRook),
    )

    assert(parseBoardString(input) == expected)
  }
}
