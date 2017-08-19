package com.example.zhiruili.loganalyzer.rules

import scala.util.Try

trait IncompleteRule {
  def fullyInitialize(findRuleByName: String => Try[Rule]): Try[Rule]
}

case class RuleName(name: String) extends IncompleteRule {
  override def fullyInitialize(findRuleByName: String => Try[Rule]): Try[Rule] = findRuleByName(name)
}

trait RuleParser {
  def parseRuleString(str: String): Try[IncompleteRule]
}
