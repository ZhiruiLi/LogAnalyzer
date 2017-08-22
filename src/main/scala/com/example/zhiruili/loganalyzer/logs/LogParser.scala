package com.example.zhiruili.loganalyzer.logs

import java.io.File

import scala.io.Source
import scala.util.Try

/**
  * 日志解析器
  */
trait LogParser {

  def parseLines(lines: List[String]): Try[List[LogItem]] = {
    lines.foldLeft(Try(List.empty[LogItem])) { case (tryList, line) =>
      for {
        lst <- tryList
        log <- parseLine(line)
      } yield log::lst
    }.map(_.reverse)
  }

  def parseString(logString: String): Try[List[LogItem]] = parseLines(logString.split("""\r?\n([ \f\t]*\r?\n)*""").toList)

  def parseFile(file: File): Try[List[LogItem]] = for {
    lines <- Try { Source.fromFile(file).getLines().filterNot(_.matches("""\s*""")).toList }
    res <- parseLines(lines)
  } yield res

  def parseLine(line: String): Try[LogItem]
}
