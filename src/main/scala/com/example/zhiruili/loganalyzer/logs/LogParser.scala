package com.example.zhiruili.loganalyzer.logs

import scala.util.Try

/**
  * 日志解析器
  */
trait LogParser {
  def parseLogString(logString: String): Try[List[LogItem]]
}

object LogParser extends LogParser {

  def parseLogString(logString: String): Try[List[LogItem]] = Try {
    LogParsers.parse(LogParsers.logItems, logString).get
  }
}
