# Chess Engine Foundation Library Design Document

## Overview
This document outlines the design of a Scala library that provides foundational functionality for building chess engines. The key design goals are:
- Easy testing and debugging during development
- High performance for production chess engines
- Clean separation of board representation from move validation logic
- Comprehensive handling of all chess edge cases (en passant, castling, pins, etc.)

## Core Types and Interfaces

### Move Representation

```scala
enum PieceType {
  case KING, QUEEN, ROOK, BISHOP, KNIGHT, PAWN
}

enum SpecialMove {
  case NORMAL, EN_PASSANT, CASTLE_KINGSIDE, CASTLE_QUEENSIDE
}

// Core move interface - always uses square indices (0-63) internally
trait Move {
  def from: Int
  def to: Int
  def specialMove: SpecialMove
  def promotionPiece: Option[PieceType]  // Validated at runtime to exclude KING/PAWN
}

trait MoveFactory {
  // Core method used by move generation/validation logic
  def create(fromIndex: Int, toIndex: Int, 
             special: SpecialMove = SpecialMove.NORMAL,
             promotion: Option[PieceType] = None): Move
             
  // Developer-friendly method for testing/debugging
  def createFromAlgebraic(from: String, to: String,
                         special: SpecialMove = SpecialMove.NORMAL,
                         promotion: Option[PieceType] = None): Move

  def toDebugString(move: Move): String
}
```

### Board State

```scala
trait BoardState {
  def getPiece(square: Int): Option[(PieceType, Color)]
  def applyMove(move: Move): BoardState
}

trait BoardStateFactory {
  def createInitial(): BoardState
  def fromFEN(fen: String): BoardState
  def fromTestString(boardStr: String): BoardState  // Simplified format for tests
  def toDebugString(board: BoardState): String
}
```

### Move Generation and Validation

```scala
trait MoveGenerator {
  // Return moves lazily to allow early termination
  def getLegalMoves(board: BoardState): Iterator[Move]
  def getCaptureMoves(board: BoardState): Iterator[Move]
  def getKingMoves(board: BoardState): Iterator[Move]
  
  // Direct checks for common scenarios
  def isSquareAttacked(board: BoardState, square: Int, byColor: Color): Boolean
  def isInCheck(board: BoardState, color: Color): Boolean
}
```

## Key Design Decisions

### Square Representation
- Internally uses 0-63 indices for squares for performance
- Conversion to/from algebraic notation handled by factories
- This avoids need for generic type parameters while maintaining ease of testing

### Move Generation
- Uses iterators rather than collections for lazy evaluation
- Allows early termination for search algorithms
- Supports efficient move ordering for alpha-beta pruning

### Separation of Attack Detection
To avoid circular dependencies in move validation:
1. `isSquareAttacked` is implemented as a primitive operation that only considers piece movement patterns
2. This is used to implement pin detection and check validation
3. Full legal move generation then uses these as building blocks

### Factory Pattern
- Core logic always works with efficient integer-based representation
- Factories provide developer-friendly interfaces for testing/debugging
- Different implementations can optimize for different use cases:
    - Debug versions with readable string representations
    - Production versions working directly with indices
    - GUI versions handling screen coordinates

## Implementation Strategy

1. Start with simple, obviously correct implementations:
    - String-based board representation for testing
    - Full validation of all edge cases
    - Comprehensive unit tests

2. Once correctness is established:
    - Implement efficient bitboard-based representations
    - Optimize critical paths in move generation
    - Verify against test suite

## Edge Cases to Handle

- En passant capture opportunities
- Castling prerequisites:
    - King and rook haven't moved
    - Path is clear
    - King doesn't pass through check
- Pawn promotion
- Piece pins (absolute and relative)
- Check and checkmate detection
- Stalemate detection

## Next Steps

1. Implement core interfaces
2. Create debug implementations focused on correctness
3. Build comprehensive test suite
4. Implement efficient production versions
5. Benchmark and optimize critical paths

## Open Questions

1. Format for test board representation - FEN vs simplified notation
2. Whether to add specialized bitboard operations for performance
3. Best approach for move ordering in iterators
4. How to handle draw by repetition detection