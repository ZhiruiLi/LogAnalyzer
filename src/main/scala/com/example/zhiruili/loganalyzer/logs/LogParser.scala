package com.example.zhiruili.loganalyzer.logs

import java.io.File

import scala.io.Source
import scala.util.Try

/**
  * 日志解析器
  */
trait LogParser {

  /**
    * 解析多行日志
    *
    * @param lines  日志列表，每一个 element 为一行日志
    * @return 解析后的日志列表，可能出错
    */
  def parseLines(lines: List[String]): Try[List[LogItem]] = {
    lines.foldLeft(Try(List.empty[LogItem])) { case (tryList, line) =>
      for {
        lst <- tryList
        log <- parseLine(line)
      } yield log::lst
    }.map(_.reverse)
  }

  /**
    * 解析整个日志字符串，默认会跳过空白行
    *
    * @param logString  日志字符串
    * @return 解析后的日志列表，可能出错
    */
  def parseString(logString: String): Try[List[LogItem]] = parseLines(logString.split("""\r?\n([ \f\t]*\r?\n)*""").toList)

  /**
    * 解析日志文件，默认会跳过空白行
    *
    * @param file 日志文件
    * @return 解析后的日志列表，可能出错
    */
  def parseFile(file: File): Try[List[LogItem]] = for {
    lines <- Try { Source.fromFile(file).getLines().filterNot(_.matches("""\s*""")).toList }
    res <- parseLines(lines)
  } yield res

  /**
    * 解析一行日志
    *
    * @param line 一行日志
    * @return 解析后的日志，可能出错
    */
  def parseLine(line: String): Try[LogItem]
}
