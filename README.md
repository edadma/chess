# Scala.js Chess Engine

A modular chess engine implementation in Scala.js that supports multiple engine types with varying levels of sophistication. The project includes a basic engine and infrastructure for developing more advanced chess-playing capabilities.

## Features

- Modular chess engine design with extensible architecture
- Complete chess rule implementation including all piece movements
- Support for special moves and game states
- Two engine implementations:
    - BasicEngine: Simple material-focused evaluation
    - NestedEngine: More sophisticated minimax implementation with alpha-beta pruning
- Interactive game play through Node.js REPL
- Comprehensive test suite

## Installation

Add this to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "chess" % "0.0.1"
```

## Usage

### Playing Against the Engine

```scala
import io.github.edadma.chess._

// Create a new game
val game = new Game
game.initialize()

// Create an engine instance
val engine = new BasicEngine()

// Make moves
game.makeMove(Move(Square('e', 2), Square('e', 4)))

// Get engine's move
engine.makeMove(game).foreach(game.makeMove)
```

### Using the REPL Interface

Run the application and use algebraic notation for moves:

```bash
> e2 e4
```

## Game Class Documentation

The `Game` class is the core component that manages the chess game state and rules. Here are its main methods and behaviors:

### Constructor and Initialization

- `new Game()`: Creates a new chess game instance
- `initialize()`: Sets up the chess board with pieces in their starting positions
- `initializeFromString(layout: String, nextTurn: Color = White)`: Initializes the board from a string representation

### Game State Methods

- `getCurrentTurn: Color`: Returns the current player's color
- `getPiece(square: Square): Option[Piece]`: Returns the piece at the given square if any
- `isCheck(color: Color): Boolean`: Determines if the specified color's king is in check
- `isCheckmate(color: Color): Boolean`: Determines if the specified color is in checkmate

### Move Management

- `makeMove(move: Move): Boolean`: Attempts to make a move, returns true if successful
- `getAllLegalMoves: List[Move]`: Returns all legal moves for the current player
- `getLegalMovesForPiece(square: Square, piece: Piece): List[Move]`: Returns all legal moves for a specific piece

### Board Manipulation

- `removePiece(square: Square)`: Removes a piece from the specified square
- `placePiece(square: Square, piece: Piece)`: Places a piece on the specified square
- `boardToString(perspective: Color): String`: Returns a string representation of the board from the given perspective
- `copyFrom(other: Game)`: Copies the state from another game instance

### Move Validation

The Game class automatically handles:
- Turn order enforcement
- Legal move validation for all piece types
- Check and checkmate detection
- Pin detection (pieces pinned to king)
- Movement range validation

### Key Behaviors

1. **Turn Management**:
    - Alternates between White and Black
    - Only allows moves from the current player
    - Automatically switches turns after valid moves

2. **Move Validation**:
    - Validates piece movement patterns
    - Ensures moves don't leave or put own king in check
    - Handles piece captures correctly

3. **Game State Tracking**:
    - Maintains current board position
    - Tracks check and checkmate conditions
    - Manages piece positions and captures

## Engine Architecture

The project uses a modular engine design with the `Engine` trait:

```scala
trait Engine {
  def makeMove(game: Game): Option[Move]
}
```

### Implemented Engines

1. **BasicEngine**:
    - Simple material-based evaluation
    - Basic positional understanding
    - Immediate capture detection
    - One-move lookahead

2. **NestedEngine**:
    - Minimax algorithm implementation
    - Alpha-beta pruning
    - Configurable search depth
    - More sophisticated position evaluation

## Building and Testing

```bash
sbt compile        # Compile the project
sbt test          # Run tests
sbt fastLinkJS    # Create development JS bundle
sbt fullLinkJS    # Create production JS bundle
```

## Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is licensed under the ISC License.
