package io.github.edadma.chess

import scala.scalajs.js

@main def run(): Unit =
  val g      = new Game
  val player = new NestedEngine

  g.initialize()
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
              val List(from, to) = list map Square.fromAlgebraic

              if from.isEmpty then error("invalid <from>")
              else if to.isEmpty then error("invalid <to>")
              else if !g.makeMove(Move(from.get, to.get)) then
                error("illegal move")
                repl.displayPrompt()
              else
                println
                println(g.boardToString(White))

                if g.isCheckmate(g.getCurrentTurn) then println("Checkmate!")
                else if g.isCheck(g.getCurrentTurn) then println("Check!")

                println

                player.makeMove(g) match
                  case None => error("couldn't make a move")
                  case Some(move) =>
                    g.makeMove(move)
                    println
                    println(g.boardToString(White))

                    if g.isCheckmate(g.getCurrentTurn) then println("Checkmated!")
                    else if g.isCheck(g.getCurrentTurn) then println("In check!")

                    println
                    repl.displayPrompt()
          } catch {
            case e: js.JavaScriptException =>
              callback(e.asInstanceOf[js.Any], null)
          }
        }
    }

  repl = REPLModule.start(options)
