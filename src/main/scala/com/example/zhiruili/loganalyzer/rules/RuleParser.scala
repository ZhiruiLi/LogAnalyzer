package com.example.zhiruili.loganalyzer.rules

import scala.util.Try

/**
  * 未完全初始化的规则
  */
trait IncompleteRule {

  /**
    * 将规则完全初始化
    *
    * @param findRuleByName 根据名称查找规则的函数
    * @return 完全初始化的规则，可能出错
    */
  def fullyInitialize(findRuleByName: String => Try[Rule]): Try[Rule]
}

/**
  * 命名规则，仅有规则名，通过外部查找来进行完全初始化
  *
  * @param name 规则名
  */
case class RuleName(name: String) extends IncompleteRule {
  override def fullyInitialize(findRuleByName: String => Try[Rule]): Try[Rule] = findRuleByName(name)
}

/**
  * 规则解析器
  */
trait RuleParser {

  /**
    * 解析规则字符串，获取一个未完全初始化的规则
    *
    * @param str  规则字符串
    * @return 未完全初始化的规则，可能出错
    */
  def parseRuleString(str: String): Try[IncompleteRule]
}
