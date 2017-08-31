package com.example.zhiruili.loganalyzer.logs

import scala.util.{Failure, Success, Try}

/**
  * 日志解析器的代理链，通过将一个 Parser 和多个 Parser 组合，
  * 实现当第一个 Parser 解析失败后由代理 Parsers 依次解析，直到解析成功或没有更多的 Parser
  */
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
