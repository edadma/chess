package io.github.edadma.chess

import scala.scalajs.js

@main def run(): Unit =
  val g        = new Game
  val player   = new SimpleEngine
  var gameOver = false

  // Show initial board
  println(g.boardToString(White))
  println()

  def error(msg: String): Unit = println(Console.RED ++ s"$msg\n" ++ Console.RESET)

  var repl: REPLServer = null

  val options: REPLOptions =
    new REPLOptions {
      prompt = "> "
      eval = (cmd: String, context: js.Object, filename: String, callback: js.Function2[js.Any, js.Any, Unit]) => {
        if (gameOver) {
          println("Game is over! Start a new game.")
          repl.displayPrompt()
        } else {
          try {
            val input = cmd.trim

            if (input.toLowerCase == "quit" || input.toLowerCase == "exit") {
              println("Thanks for playing!")
              repl.close()
            } else {
              val list = input.split(" ").toList

              if (list.length != 2) {
                error("Expected '<from> <to>' (e.g. 'e2 e4')")
                repl.displayPrompt()
              } else {
                val List(from, to) = list
                val piece          = g.getBoard.getPiece(fromAlgebraic(from))

                try {
                  val move = g.moveFactory.create(
                    fromAlgebraic(from),
                    fromAlgebraic(to),
                    piece.get,
                    MoveType.NORMAL,
                    None,
                  )

                  if (piece.isEmpty || !g.makeMove(move)) {
                    error("Illegal move")
                    repl.displayPrompt()
                  } else {
                    println()
                    println(s"Your move: ${g.lastMoveToSAN}")
                    println(g.boardToString(White))

                    if (g.isCheckmate) {
                      println("Checkmate - You win!")
                      gameOver = true
                      repl.displayPrompt()
                    } else {
                      if (g.isInCheck) {
                        println("Check!")
                      }
                      println()

                      // Computer's move
                      player.makeMove(g) match {
                        case None =>
                          error("Computer couldn't make a move")
                          repl.displayPrompt()
                        case Some(move) =>
                          g.makeMove(move)
                          println(s"Computer's move: ${g.lastMoveToSAN}")
                          println()
                          println(g.boardToString(White))

                          if (g.isCheckmate) {
                            println("Checkmate - Computer wins!")
                            gameOver = true
                          } else if (g.isInCheck) {
                            println("You are in check!")
                          }
                          println()
                          repl.displayPrompt()
                      }
                    }
                  }
                } catch {
                  case e: IllegalArgumentException =>
                    error(s"Invalid move format: ${e.getMessage}")
                    repl.displayPrompt()
                }
              }
            }
          } catch {
            case e: js.JavaScriptException =>
              callback(e.asInstanceOf[js.Any], null)
            case e: Exception =>
              error(s"Error: ${e.getMessage}")
              repl.displayPrompt()
          }
        }
      }
    }

  println("Welcome to Chess! Enter moves in the format 'e2 e4'")
  println("Type 'quit' or 'exit' to end the game")
  println()

  repl = REPLModule.start(options)
