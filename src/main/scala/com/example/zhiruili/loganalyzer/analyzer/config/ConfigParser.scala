package com.example.zhiruili.loganalyzer.analyzer.config

import scala.util.Try

/**
  * 配置文件解析器
  */
trait ConfigParser {
  def parseConfigString(initStr: String): Try[AnalyzerConfig]
}
