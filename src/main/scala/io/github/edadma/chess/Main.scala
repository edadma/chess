package io.github.edadma.chess

import scala.scalajs.js

@main def run(): Unit =
  val g = new Game

  g.initialize()

  val options = new REPLOptions {
    prompt = "\n" ++ g.boardToString(White) ++ "\n\n" ++ "> "
    eval = (cmd: String, context: js.Object, filename: String, callback: js.Function2[js.Any, js.Any, Unit]) => {
      try {
        val list = cmd.trim.split(" ").toList

        if list.length != 2 then callback(null, "expected '<from> <to>'")
        else
          val List(from, to) = list map Square.fromAlgebraic

          if from.isEmpty then callback(null, "invalid <from>")
          else if to.isEmpty then callback(null, "invalid <to>")
          else if !g.makeMove(Move(from.get, to.get)) then callback(null, "illegal move")
          else callback(null, "move executed")
      } catch {
        case e: js.JavaScriptException =>
          callback(e.asInstanceOf[js.Any], null)
      }
    }
  }

  val repl = REPLModule.start(options)

//  val sayHelloCommand = new REPLCommand {
//    help = "Say hello"
//    action = (name: String) => {
//      println(s"Hello, $name!")
//      repl.displayPrompt()
//    }
//  }
//
//  repl.defineCommand("sayhello", sayHelloCommand)
