package com.example.zhiruili.loganalyzer.analyzer

import com.example.zhiruili.loganalyzer.analyzer.config.AnalyzerConfig.{HelpInfoBinding, Problem}
import com.example.zhiruili.loganalyzer.analyzer.config.{ConfigLoader, RootConfig}
import com.example.zhiruili.loganalyzer.rules.{Rule, RuleLoader}
import com.example.zhiruili.loganalyzer.{Platform, Sdk, Version}

import scala.util.Try

/**
  * 日志分析器的加载器
  */
trait LogAnalyzerLoader {

  /**
    * 配置读取器
    */
  def configLoader: ConfigLoader

  /**
    * 规则加载器
    */
  def ruleLoader: RuleLoader

  /**
    * 读取一条规则
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @param ruleName   规则名
    * @return 规则，可能失败
    */
  def loadRule(sdk: Sdk, platform: Platform, version: Version, ruleName: String): Try[Rule] = {
    ruleLoader.loadRule(sdk, platform, version)(ruleName)
  }

  /**
    * 加载日志分析器
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @return 对应的日志分析器，可能出错
    */
  def loadAnalyzer(sdk: Sdk, platform: Platform, version: Version): Try[LogAnalyzer] = {
    configLoader.loadBaseConfig(sdk, platform, version).map {
      case RootConfig(problemBindings) =>
        new LogAnalyzer {
          val problemCodeMap: Map[Int, List[HelpInfoBinding]] = problemBindings.map {
            case (Problem(code, _), helpBindings) => (code, helpBindings)
          }.toMap
          override def loadHelpInfoBindings(problemCode: Int): Option[List[HelpInfoBinding]] = {
            problemCodeMap.get(problemCode)
          }
          override def loadRuleByName(ruleName: String): Try[Rule] = loadRule(sdk, platform, version, ruleName)
        }
    }
  }
}

object LogAnalyzerLoader {

  def apply(configLd: ConfigLoader, ruleLd: RuleLoader): LogAnalyzerLoader = {
    new LogAnalyzerLoader {
      override def configLoader: ConfigLoader = configLd
      override def ruleLoader: RuleLoader = ruleLd
    }
  }
}
