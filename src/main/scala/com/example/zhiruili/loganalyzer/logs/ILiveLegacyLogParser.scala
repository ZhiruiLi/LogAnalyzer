package com.example.zhiruili.loganalyzer.logs

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object ILiveLegacyLogParser extends LogParser {

  type ThreadId = Int
  type Position = String
  type ModuleOrKeyTag = String
  type Message = String

  def parseLine(line: String) = Try {
    ILiveLegacyLogParsers.parseAll(ILiveLegacyLogParsers.potentialLog, line).map {
      case (time, lv, tId, pos, modOrKey, msg) =>
        val (optIsKey, optModule) = if (modOrKey == "Key_Procedure") (Some(true), None) else (Some(false), Some(modOrKey))
        val optErrCode = findErrorCode(msg)
        val actualLv = optErrCode.map(_ => LvError).getOrElse(lv)
        val extMap = Map("threadId" -> tId.toString, "module" -> optModule.getOrElse(""))
        EquivocalLog(line, Some(time), optIsKey, Some(actualLv), Some(pos), Some(msg), extMap)
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
      * 解析是否是关键路径日志，取值仅为 KEY 或 DEV
      */
    def isKeyLog: Parser[Boolean] = ("KEY" | "DEV") ^^ {
      case "KEY" => true
      case "DEV" => false
    }

    /**
      * 任意不包含竖线的字符串
      */
    def noBarString: Parser[String] = """[^\|]+""".r ^^ (_.trim)

    /**
      * 解析打印日志的位置
      */
    def position: Parser[String] = noBarString

    /**
      * 解析日志打印模块，关键日志解析也在此解析
      */
    def module: Parser[String] = noBarString

    /**
      * 解析日志信息，任意字符串
      */
    def message: Parser[String] = """.*""".r ^^ (_.trim)

    /**
      * 解析线程 ID
      */
    def threadId: Parser[Int] = """\d+""".r ^^ (_.toInt)

    /**
      * 解析模糊日志
      */
    def potentialLog: Parser[(Date, LogLevel, ThreadId, Position, ModuleOrKeyTag, Message)] = {

      def bSep[T](parser: Parser[T]): Parser[T] = parser <~ "|"

      bSep(timestamp) ~ bSep(level) ~ bSep(threadId) ~ bSep(position) ~ bSep(module) ~ message ^^ {
        case time ~ lv ~ tId ~ pos ~ mod ~ msg => (time, lv, tId, pos, mod, msg)
      }
    }
  }

  object ILiveLegacyLogParsers extends ILiveLegacyLogParsers
}
