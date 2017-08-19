package com.example.zhiruili.loganalyzer.logs

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * 日志文件解析器组合子
  */
trait LogParsers extends RegexParsers {

  override val whiteSpace: Regex = """[ \f\t]+""".r
  val eol: Regex = """((\r?\n)+)|$""".r
  lazy val yearHead: String = (Calendar.getInstance.get(Calendar.YEAR) / 100).toString

  def timestampStr: Parser[String] =
    ("""\d{4}""".r | """\d{2}""".r) ~ """-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}""".r ^^ {
      case head ~ tail =>
        if (head.length == 4) head + tail else yearHead + head + tail
    }

  /**
    * 解析时间戳
    * 支持格式 yyyy-MM-dd HH:mm:ss 以及 yy-MM-dd HH:mm:ss
    */
  def timestamp: Parser[Date] = timestampStr ^^ new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse

  /**
    * 解析是否是关键路径日志，取值仅为 KEY 或 DEV
    */
  def isKeyLog: Parser[Boolean] = """(KEY)|(DEV)""".r ^^ {
      case "KEY" => true
      case "DEV" => false
    }

  /**
    * 解析日志等级，取值仅为 D 或 I 或 W 或 E
    */
  def level: Parser[LogLevel] = ("D" | "I" | "W" | "E") ^^ {
    case "D" => LvDebug
    case "I" => LvInfo
    case "W" => LvWarn
    case "E" => LvError
  }

  /**
    * 解析任意非方括号字符串
    */
  def anyString: Parser[String] = """[^\[\]]+""".r ^^ (_.trim)

  /**
    * 解析打印日志的位置
    */
  def position: Parser[String] = anyString

  /**
    * 解析日志信息
    */
  def message: Parser[String] = anyString

  /**
    * 解析键值对
    * 形式为 Key:Value
    */
  def kvPair: Parser[(String, String)] = {
    def legalKey: Parser[String] = """[^\[\]\|:]+""".r
    def legalVal: Parser[String] = """[^\[\]\|]*""".r
    legalKey ~ ((":" ~> legalVal) | success("")) ^^ { case k ~ v => (k.trim, v.trim) }
  }

  /**
    * 解析额外信息，用 |(竖线) 分割
    */
  def extMessage: Parser[Map[String, String]] = rep1sep(kvPair, "|") ^^ (_.toMap)

  /**
    * 解析合法日志
    */
  def legalLog: Parser[LegalLog] = {

    def bracket[T](parser: Parser[T]) = "[" ~> parser <~ "]"

    def legalLogPrefix =
      bracket(timestamp) ~ bracket(isKeyLog) ~ bracket(level) ~ bracket(position) ~ bracket(message)

    legalLogPrefix ~ (bracket(extMessage) | success(Map.empty[String, String])) ^^ {
      case time ~ isKey ~ lv ~ pos ~ msg ~ ext => LegalLog(time, isKey, lv, pos, msg, ext)
    }
  }

  /**
    * 解析任意日志
    */
  def anyLog: Parser[UnknownLog] = """.+""".r ^^ UnknownLog

  /**
    * 解析单条日志，如果不是合法日志则被解析为未知日志
    */
  def logItem: Parser[LogItem] = (legalLog <~ eol) | (anyLog <~ eol)

  /**
    * 解析全部日志
    */
  def logItems: Parser[List[LogItem]] = phrase(rep(logItem))
}

object LogParsers extends LogParsers {
}
