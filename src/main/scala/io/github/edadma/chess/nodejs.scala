package io.github.edadma.chess

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

@js.native
@JSImport("repl", JSImport.Namespace)
object REPLModule extends js.Object {
  def start(options: REPLOptions = js.native): REPLServer = js.native
}

@js.native
trait REPLServer extends js.Object {
  def close(): Unit                                            = js.native
  def displayPrompt(preserveCursor: Boolean = js.native): Unit = js.native
  def defineCommand(keyword: String, cmd: REPLCommand): Unit   = js.native
}

trait REPLOptions extends js.Object {
  var prompt: js.UndefOr[String]                                                                          = js.undefined
  var input: js.UndefOr[js.Object]                                                                        = js.undefined
  var output: js.UndefOr[js.Object]                                                                       = js.undefined
  var terminal: js.UndefOr[Boolean]                                                                       = js.undefined
  var eval: js.UndefOr[js.Function4[String, js.Object, String, js.Function2[js.Any, js.Any, Unit], Unit]] = js.undefined
  var writer: js.UndefOr[js.Function1[js.Any, String]]                                                    = js.undefined
  var completer: js.UndefOr[js.Function2[String, js.Function2[js.Any, js.Array[String], Unit], Unit]]     = js.undefined
  var replMode: js.UndefOr[js.Any]                                                                        = js.undefined
  var breakEvalOnSigint: js.UndefOr[Boolean]                                                              = js.undefined
}

trait REPLCommand extends js.Object {
  var help: js.UndefOr[String]                         = js.undefined
  var action: js.UndefOr[js.Function1[String, js.Any]] = js.undefined
}
