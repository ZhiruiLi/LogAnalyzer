package com.example.zhiruili.loganalyzer.rules

import com.example.zhiruili.loganalyzer.rules.RuleLoader.RecursiveLoadRuleException
import com.example.zhiruili.loganalyzer.{Platform, Sdk, Version}

import scala.util.{Failure, Success, Try}

/**
  * 规则加载器
  */
trait RuleLoader {

  /**
    * 规则解析器
    */
  def parser: RuleParser

  /**
    * 根据规则名称加载规则
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @param ruleName   规则名
    * @return   规则，可能出错
    */
  def loadRule(sdk: Sdk, platform: Platform, version: Version)(ruleName: String): Try[Rule] = {

    // 防止递归读取
    def doLoad(ruleName: String, rulesHaveLoaded: Set[String]): Try[Rule] = {
      if (rulesHaveLoaded(ruleName)) {
        Failure(RecursiveLoadRuleException(sdk, platform, version, ruleName))
      } else {
        loadIncompleteRule(sdk, platform, version)(ruleName) match {
          case Failure(thw) => Failure(thw)
          case Success(incompleteRule) =>
            incompleteRule.fullyInitialize(nextRuleName => doLoad(nextRuleName, rulesHaveLoaded + ruleName))
        }
      }
    }
    doLoad(ruleName, Set.empty)
  }

  /**
    * 读取未初始化的规则
    *
    * @param sdk        SDK 名
    * @param platform   运行平台
    * @param version    版本号
    * @param ruleName   规则名
    * @return 未初始化的规则，可能出错
    */
  def loadIncompleteRule(sdk: Sdk, platform: Platform, version: Version)(ruleName: String): Try[IncompleteRule]
}

object RuleLoader {

  /**
    * 递归初始化规则异常
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @param ruleName   被递归初始化的规则名
    */
  case class RecursiveLoadRuleException(sdk: Sdk, platform: Platform, version: Version, ruleName: String)
    extends RuntimeException(s"Recursive loading rule: $ruleName(SDK: $sdk, platform: $platform, version: $version)")

  /**
    * 找不到对应的规则
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @param ruleName   规则名
    */
  case class NoSuchRuleException(sdk: Sdk, platform: Platform, version: Version, ruleName: String)
    extends RuntimeException(s"No such rule named: $ruleName(SDK: $sdk, platform: $platform, version: $version)")

  /**
    * 规则读取异常
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @param ruleName   规则名
    * @param thw        异常
    */
  case class RuleLoadingException(sdk: Sdk, platform: Platform, version: Version, ruleName: String, thw: Throwable)
    extends RuntimeException(s"Rule '$ruleName' loading error: ${thw.getMessage}(SDK: $sdk, platform: $platform, version: $version)")
}
