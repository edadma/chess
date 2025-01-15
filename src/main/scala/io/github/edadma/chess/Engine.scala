package io.github.edadma.chess

// Trait defining the interface for all chess engines
trait Engine {
  def makeMove(game: Game): Option[Move]
}
