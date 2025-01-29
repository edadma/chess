package io.github.edadma.chess

import scala.scalajs.js

@main def run(): Unit =
  val g      = new Game
  val player = new SimpleEngine

  println(g.boardToString(White))
  println

  def error(msg: String): Unit = println(Console.RED ++ s"$msg\n" ++ Console.RESET)

  var repl: REPLServer = null

  val options: REPLOptions =
    new REPLOptions {
      prompt = "> "
      eval =
        (cmd: String, context: js.Object, filename: String, callback: js.Function2[js.Any, js.Any, Unit]) => {
          try {
            val list = cmd.trim.split(" ").toList

            if list.length != 2 then callback(null, "expected '<from> <to>'")
            else
              val List(from, to) = list

              if !g.makeMove(UserMove(from, to, g.getBoard)) then
                error("illegal move")
                repl.displayPrompt()
              else
                println
                println(g.boardToString(White))

                if g.isCheckmate then println("Checkmate!")
                else if g.isInCheck then println("Check!")

                println

                player.makeMove(g) match
                  case None => error("couldn't make a move")
                  case Some(move) =>
                    g.makeMove(move)
                    println
                    println(g.boardToString(White))

                    if g.isCheckmate then println("Checkmated!")
                    else if g.isInCheck then println("In check!")

                    println
                    repl.displayPrompt()
          } catch {
            case e: js.JavaScriptException =>
              callback(e.asInstanceOf[js.Any], null)
          }
        }
    }

  repl = REPLModule.start(options)
