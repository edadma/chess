package io.github.edadma.chess

import io.github.edadma.logger.*
import org.scalatest.BeforeAndAfterEach
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ChessSpec extends AnyFreeSpec with Matchers with BeforeAndAfterEach:
  override def beforeEach(): Unit = {
    super.beforeEach()

    logger.setLogLevel(LogLevel.OFF)
    logger.resetOpId()
  }

  def withDebugLogging(testName: String)(test: => Unit): Unit = {
    logger.setLogLevel(LogLevel.DEBUG)
    logger.setHandler(new FileHandler(s"${System.currentTimeMillis}.log"))
    logger.debug(s"<<<< Starting Test: $testName >>>>", category = "Test")

    try {
      test
    } finally {
      logger.setLogLevel(LogLevel.OFF)
    }
  }
