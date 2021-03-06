package com.example.zhiruili.loganalyzer.analyzer

import com.example.zhiruili.loganalyzer.Version
import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.{HelpInfoBinding, ProblemTag}

/**
  * 日志分析器的配置
  */
sealed trait AnalyzerConfig

/**
  * 基础配置
  *
  * @param problemBindings  问题和规则与帮助信息的绑定关系
  */
case class RootConfig(problemBindings: List[(ProblemTag, List[HelpInfoBinding])]) extends AnalyzerConfig

/**
  * 继承配置
  *
  * @param fromVersion    继承源版本号
  * @param currConfig     当前配置
  */
case class ExtendConfig(fromVersion: Version, currConfig: RootConfig) extends AnalyzerConfig

object AnalyzerConfig {

  /**
    * 问题标签
    *
    * @param value  tag 值
    */
  case class ProblemTag(value: String)

  /**
    * 问题
    *
    * @param tag    问题标签，唯一
    * @param name   问题名称
    */
  case class Problem(tag: ProblemTag, name: String)

  /**
    * 分析成功的帮助提示
    *
    * @param message    帮助信息
    * @param helpPage   相关帮助链接
    */
  case class HelpInfo(message: String, helpPage: Option[String])

  /**
    * 匹配规则与帮助提示的绑定
    *
    * @param ruleName   规则名
    * @param helpInfo   帮助提示
    */
  case class HelpInfoBinding(ruleName: String, helpInfo: HelpInfo)
}

