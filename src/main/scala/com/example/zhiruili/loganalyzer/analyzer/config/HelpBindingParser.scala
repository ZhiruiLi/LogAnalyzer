package com.example.zhiruili.loganalyzer.analyzer.config

import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig

import scala.util.Try

/**
  * 帮助绑定配置文件解析器
  */
trait HelpBindingParser {
  def parseConfigString(str: String): Try[AnalyzerConfig]
}
