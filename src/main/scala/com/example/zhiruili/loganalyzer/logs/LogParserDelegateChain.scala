package com.example.zhiruili.loganalyzer.logs

import scala.util.{Success, Try}

trait LogParserDelegateChain extends LogParser {

  def basicParser: LogParser
  def delegateParsers: List[LogParser]

  def delegateParsing(logStr: String, parsers: List[LogParser]): Option[List[LogItem]] = {
    parsers match {
      case Nil => None
      case parser::remain => parser.parseLogString(logStr) match {
        case Success(logs) => Some(logs)
        case _ => delegateParsing(logStr, remain)
      }
    }
  }

  def parseUnknown(unknownLog: UnknownLog): List[LogItem] = {
    delegateParsing(unknownLog.originalLog, delegateParsers) match {
      case Some(logs) => logs
      case None => List(unknownLog)
    }
  }

  override def parseLogString(logString: String): Try[List[LogItem]] = {
    basicParser
      .parseLogString(logString)
      .map { logs =>
        logs.view.flatMap {
          case log@UnknownLog(_) =>
            parseUnknown(log)
          case log => List(log)
        }.toList
      }
  }
}
