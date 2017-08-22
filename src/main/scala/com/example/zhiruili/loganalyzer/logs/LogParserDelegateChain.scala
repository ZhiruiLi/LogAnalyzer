package com.example.zhiruili.loganalyzer.logs

import scala.util.{Failure, Success, Try}

trait LogParserDelegateChain extends LogParser {

  def basicParser: LogParser
  def delegateParsers: List[LogParser]

  def delegateParsing(line: String, parsers: List[LogParser]): Option[LogItem] = {
    parsers match {
      case Nil => None
      case parser::remain => parser.parseLine(line) match {
        case Failure(_) => delegateParsing(line, remain)
        case Success(UnknownLog(_)) => delegateParsing(line, remain)
        case Success(log) => Some(log)
      }
    }
  }

  def parseUnknown(unknownLog: UnknownLog): LogItem = {
    delegateParsing(unknownLog.originalLog, delegateParsers).getOrElse(unknownLog)
  }

  override def parseLine(line: String): Try[LogItem] = {
    basicParser.parseLine(line).map {
      case log: UnknownLog => parseUnknown(log)
      case log => log
    }
  }
}

object LogParserDelegateChain {
  def apply(basicLogParser: LogParser, delegateChain: List[LogParser]): LogParserDelegateChain =
    new LogParserDelegateChain {
      val basicParser: LogParser = basicLogParser
      val delegateParsers: List[LogParser] = delegateChain
    }
}
