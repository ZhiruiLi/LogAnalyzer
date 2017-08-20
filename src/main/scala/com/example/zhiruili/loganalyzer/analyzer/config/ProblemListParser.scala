package com.example.zhiruili.loganalyzer.analyzer.config

import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.Problem

import scala.util.Try

/**
  * 问题列表配置文件解析器
  */
trait ProblemListParser {
  def parseConfigString(str: String): Try[List[Problem]]
}
