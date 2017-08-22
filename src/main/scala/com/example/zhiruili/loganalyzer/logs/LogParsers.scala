package com.example.zhiruili.loganalyzer.logs

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait LogParsers extends RegexParsers {
  override val whiteSpace: Regex = """[ \f\t]+""".r
  val eol: Regex = """((\r?\n)+)|$""".r
}
