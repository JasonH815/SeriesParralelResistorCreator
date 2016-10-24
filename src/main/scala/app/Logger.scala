package app

import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, LoggerContext}
import ch.qos.logback.core.ConsoleAppender
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory


/**
  * Created by Jason on 10/23/2016.
  */
object LoggerConfig {
  val logLevel = Level.INFO
}

object GlobalLogger {

    val lc:LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val ple:PatternLayoutEncoder = new PatternLayoutEncoder()

    ple.setPattern("%date %level [%thread] %logger{10} %class{0} [%file:%line] %msg%n")
    ple.setContext(lc)
    ple.start()
    val consoleAppender: ConsoleAppender[ILoggingEvent] = new ConsoleAppender[ILoggingEvent]()
    consoleAppender.setEncoder(ple)
    consoleAppender.setContext(lc)
    consoleAppender.start()

    //FileAppender<ILoggingEvent> fileAppender = new FileAppender<ILoggingEvent>();
    //fileAppender.setFile(file);
    //fileAppender.setEncoder(ple);
    //fileAppender.setContext(lc);
    //fileAppender.start();

    val logger = Logger(LoggerFactory.getLogger("logger"))
    logger.underlying.asInstanceOf[ch.qos.logback.classic.Logger].setLevel(LoggerConfig.logLevel)
    logger.underlying.asInstanceOf[ch.qos.logback.classic.Logger].addAppender(consoleAppender)
    logger.underlying.asInstanceOf[ch.qos.logback.classic.Logger].setAdditive(false)

    logger.info("Global logger started")
}