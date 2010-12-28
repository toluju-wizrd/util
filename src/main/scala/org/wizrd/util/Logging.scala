package org.wizrd.util

import java.io.{PrintWriter, OutputStream}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.logging.LogManager
import org.slf4j.bridge.SLF4JBridgeHandler
import org.slf4j.{Logger => SLF4JLogger, LoggerFactory}
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.jul.LevelChangePropagator
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.spi.ThrowableProxy
import ch.qos.logback.core.OutputStreamAppender
import ch.qos.logback.core.encoder.EncoderBase
import ch.qos.logback.classic.Level
import scala.math.ScalaNumber
import scala.collection.JavaConversions._

trait Logging {
  protected def logClass:Class[_] = this.getClass
  protected def logName:String = null
  
  protected lazy val log = if (logName == null) Logger(logClass) else Logger(logName)
  protected implicit def loggableThrowable(t:Throwable) = new LoggableThrowable(log.log, t)

  protected class LoggableThrowable(val log:SLF4JLogger, val t:Throwable) {
    def logError(msg: => String, args:Any*) = 
      if (log.isErrorEnabled) log.error(msg.format(args map Logger.unwrapArg:_*), t)
    def logWarn(msg: => String, args:Any*) = 
      if (log.isWarnEnabled) log.warn(msg.format(args map Logger.unwrapArg:_*), t)
    def logInfo(msg: => String, args:Any*) = 
      if (log.isInfoEnabled) log.info(msg.format(args map Logger.unwrapArg:_*), t)
    def logDebug(msg: => String, args:Any*) = 
      if (log.isDebugEnabled) log.debug(msg.format(args map Logger.unwrapArg:_*), t)
    def logTrace(msg: => String, args:Any*) = 
      if (log.isTraceEnabled) log.trace(msg.format(args map Logger.unwrapArg:_*), t)
  }
}

object Logger {
  private val badProperties = Set("sun.os.patch.level")
  private var isInit = false

  def init(levels:Map[String,String] = Map.empty):Unit = {
    if (!isInit) {
      val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
      lc.reset

      val encoder = new ColorfulEncoder
      encoder.setContext(lc)
      encoder.init(System.err)
      encoder.start
      
      val appender = new OutputStreamAppender[ILoggingEvent]
      appender.setContext(lc)
      appender.setOutputStream(System.err)
      appender.setEncoder(encoder)
      appender.start

      val root = lc.getLogger(SLF4JLogger.ROOT_LOGGER_NAME)
      root.setLevel(Level.INFO)
      root.addAppender(appender)

      levels.foreach(t => {
        val (name, value) = t
        lc.getLogger(name).setLevel(Level.toLevel(value, Level.INFO))
        println("Set logging level: %s = %s".format(name, value))
      })

      System.getProperties.foreach(p => {
        val (key, value) = p
        if (!badProperties.contains(key) && key.endsWith(".level")) {
          val name = key.substring(0, key.length - ".level".length)
          lc.getLogger(name).setLevel(Level.toLevel(value, Level.INFO))
          println("Set logging level: %s = %s".format(name, value))
        }
      })

      LogManager.getLogManager.reset
      val julFix = new LevelChangePropagator
      julFix.setContext(lc)
      julFix.resetJULLevels
      julFix.start
      lc.addListener(julFix)
      SLF4JBridgeHandler.install()

      isInit = true
      apply(this.getClass).info("Logging initialized")
    }
  }

  def apply(name:String):Logger =
    new DefaultLogger(LoggerFactory getLogger name)

  def apply(clazz:Class[_]):Logger =
    apply(clazz.getName.takeWhile(_ != '$'))
  
  def apply():Logger = 
    apply(new Throwable().getStackTrace()(0).getClassName)

  private[util] def unwrapArg(arg:Any):AnyRef = arg match {
    case x:ScalaNumber => x.underlying
    case x             => x.asInstanceOf[AnyRef]
  }

  private class DefaultLogger(override protected[util] val log:SLF4JLogger)
          extends Logger
}

trait Logger {
  protected[util] val log:SLF4JLogger
  lazy val name = log.getName

  def error(msg: => String, args:Any*) = 
    if (log.isErrorEnabled) log.error(msg.format(args map Logger.unwrapArg:_*))
  def warn(msg: => String, args:Any*) = 
    if (log.isWarnEnabled) log.warn(msg.format(args map Logger.unwrapArg:_*))
  def info(msg: => String, args:Any*) = 
    if (log.isInfoEnabled) log.info(msg.format(args map Logger.unwrapArg:_*))
  def debug(msg: => String, args:Any*) = 
    if (log.isDebugEnabled) log.debug(msg.format(args map Logger.unwrapArg:_*))
  def trace(msg: => String, args:Any*) = 
    if (log.isTraceEnabled) log.trace(msg.format(args map Logger.unwrapArg:_*))
}

object LogTest extends Logging {
  def main(args:Array[String]) = {
    println("Starting logging")
    Logger.init()
    log.info("Hello world")
    log.info("Hello %s", "world")
    log.debug("Debug")
    log.trace("Trace")
    val t = new Throwable("msg")
    t.logWarn("Throwable here")
    t.logWarn("Throwable here, %d", 10)
  }
}

class ColorfulEncoder extends EncoderBase[ILoggingEvent] {
  var writer:PrintWriter = _
  val dateFormat = new SimpleDateFormat("HH:mm:ss")

  val levelColors = Map(
    Level.ERROR -> List(colors.RED, colors.BRIGHT).mkString,
    Level.WARN -> List(colors.RED).mkString,
    Level.INFO -> List(colors.GREEN).mkString,
    Level.DEBUG -> List(colors.CYAN).mkString,
    Level.TRACE -> List(colors.MAGENTA).mkString
  )

  override def init(out:OutputStream) = {
    super.init(out)
    writer = new PrintWriter(out)
  }

  def close() = Unit

  def doEncode(event:ILoggingEvent) = {
    writer.append(levelColors.getOrElse(event.getLevel, "")).append("[")
          .append(dateFormat.format(new Date(event.getTimeStamp))).append(" ")
          .append(event.getLoggerName.reverse.takeWhile(_ != '.').reverse)
          .append("]").append(colors.RESET.code).append(" ")
          .append(event.getFormattedMessage).append("\n").flush

    event.getThrowableProxy match {
      case t:ThrowableProxy =>
        t.getThrowable.printStackTrace(writer)
    }
  }
}

sealed abstract class TerminalColor(val code:String) {
  override def toString = code
}

package colors {
  case object NONE          extends TerminalColor("")
  case object RESET         extends TerminalColor("\033[0m")
  case object BRIGHT        extends TerminalColor("\033[1m")
  case object DIM           extends TerminalColor("\033[2m")
  case object UNDERLINE     extends TerminalColor("\033[4m")
  case object BLINK         extends TerminalColor("\033[5m")
  case object REVERSE       extends TerminalColor("\033[7m")
  case object HIDDEN        extends TerminalColor("\033[8m")

  case object DEFAULT_FG    extends TerminalColor("\033[39m")
  case object BLACK         extends TerminalColor("\033[30m")
  case object RED           extends TerminalColor("\033[31m")
  case object GREEN         extends TerminalColor("\033[32m")
  case object YELLOW        extends TerminalColor("\033[33m")
  case object BLUE          extends TerminalColor("\033[34m")
  case object MAGENTA       extends TerminalColor("\033[35m")
  case object CYAN          extends TerminalColor("\033[36m")
  case object LIGHT_GRAY    extends TerminalColor("\033[37m")

  case object NONE_BG       extends TerminalColor("")
  case object DEFAULT_BG    extends TerminalColor("\033[49m")
  case object RED_BG        extends TerminalColor("\033[41m")
  case object GREEN_BG      extends TerminalColor("\033[42m")
  case object YELLOW_BG     extends TerminalColor("\033[43m")
  case object BLUE_BG       extends TerminalColor("\033[44m")
  case object MAGENTA_BG    extends TerminalColor("\033[45m")
  case object CYAN_BG       extends TerminalColor("\033[46m")
  case object LIGHT_GRAY_BG extends TerminalColor("\033[47m")
}
