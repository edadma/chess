package io.github.edadma.chess

import scala.scalajs.js

@main def run(): Unit =
  val g      = new Game
  val player = new BasicEngine

  g.initialize()
  println(g.boardToString(White))
  println

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
              val List(from, to) = list map Square.fromAlgebraic

              if from.isEmpty then callback(null, "invalid <from>")
              else if to.isEmpty then callback(null, "invalid <to>")
              else if !g.makeMove(Move(from.get, to.get)) then
                println(Console.RED ++ "illegal move\n" ++ Console.RESET)
                repl.displayPrompt()
              else
                println
                println(g.boardToString(White))
                println
                player.makeMove(g)
                println
                println(g.boardToString(White))
                println
                repl.displayPrompt()
          } catch {
            case e: js.JavaScriptException =>
              callback(e.asInstanceOf[js.Any], null)
          }
        }
    }

  repl = REPLModule.start(options)
