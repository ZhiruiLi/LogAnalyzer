package com.example.zhiruili.loganalyzer.logs

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object ILiveLegacyLogParser extends LogParser {

  type ThreadId = Int
  type Position = String
  type Message = String

  val threadIdTag = "threadId"

  /**
    * 解析遗留 iLive SDK 日志
    * 非关键路径日志格式为：
    * timestamp | log level | thread ID | position | message
    * 关键路径日志格式为：
    * timestamp | log level | thread ID | position | "Key_Procedure" | message
    *
    * @param line 一行日志
    * @return 一个日志项，可能出错
    */
  def parseLine(line: String): Try[LogItem] = Try {
    ILiveLegacyLogParsers.parseAll(ILiveLegacyLogParsers.potentialLog, line).map {
      case (time, lv, tId, pos, isKey, msg) =>
        val optErrCode = findErrorCode(msg)
        val actualLv = optErrCode.map(_ => LvError).getOrElse(lv)
        val ext = (threadIdTag -> tId.toString)::optErrCode.map(code => LogItem.errCodeTag -> code.toString).toList
        EquivocalLog(line, Some(time), Some(isKey), Some(actualLv), Some(pos), Some(msg), ext.toMap)
    }.getOrElse(UnknownLog(line))
  }

  def findErrorCode(message: Message): Option[Int] = {
    val strs = message.split("""([Ee]rror)|([Ff]ail)|([Ee]rrCode)""", 2)
    if (strs.length == 2) {
      """\d+""".r.findFirstIn(strs(1)).map(_.toInt)
    } else {
      None
    }
  }

  trait ILiveLegacyLogParsers extends RegexParsers {

    lazy val yearHead: String = (Calendar.getInstance.get(Calendar.YEAR) / 100).toString

    def timestampStr: Parser[String] = """\d{2}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}""".r ^^ (yearHead + _)

    /**
      * 解析时间戳
      * 支持格式 yyyy-MM-dd HH:mm:ss 以及 yy-MM-dd HH:mm:ss
      */
    def timestamp: Parser[Date] = timestampStr ^^ new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse

    /**
      * 解析日志等级，取值仅为 V (verbose) 或 D (debug) 或 I (info) 或 W (warn) 或 E (error)，但实际日志等级可能与此不同
      */
    def level: Parser[LogLevel] = ("V" | "D" | "I" | "W" | "E") ^^ {
      case "V" => LvVerbose
      case "D" => LvDebug
      case "I" => LvInfo
      case "W" => LvWarn
      case "E" => LvError
    }

    /**
      * 解析打印日志的位置，任意不包含竖线的字符串
      */
    def position: Parser[String] = """[^\|]+""".r ^^ (_.trim)

    /**
      * 解析日志信息，任意字符串
      */
    def message: Parser[String] = """.*""".r ^^ (_.trim)

    /**
      * 解析线程 ID
      */
    def threadId: Parser[Int] = """\d+""".r ^^ (_.toInt)

    /**
      * 解析是否是关键路径日志
      */
    def isKeyProcedure: Parser[Boolean] = "Key_Procedure" ^^ (_ => true)

    /**
      * 解析模糊日志
      */
    def potentialLog: Parser[(Date, LogLevel, ThreadId, Position, Boolean, Message)] = {

      def bSep[T](parser: Parser[T]): Parser[T] = parser <~ "|"

      def tail: Parser[~[Boolean, String]] = (bSep(isKeyProcedure) ~ message) | (success(false) ~ message)

      bSep(timestamp) ~ bSep(level) ~ bSep(threadId) ~ bSep(position) ~ tail ^^ {
        case time ~ lv ~ tId ~ pos ~ (isKey ~ msg) => (time, lv, tId, pos, isKey, msg)
      }
    }
  }

  object ILiveLegacyLogParsers extends ILiveLegacyLogParsers
}
