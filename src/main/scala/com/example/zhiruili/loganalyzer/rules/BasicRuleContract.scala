package com.example.zhiruili.loganalyzer.rules

object BasicRuleContract {

  object General {
    val ruleTypeKey: String = "type"
    val singleRuleKey: String = "rule"
    val multiRulesKey: String = "rules"
  }

  object AppearRule {
    val timesKey: String = "times"
    val defaultTimes: Int = 1
  }

  object SingleLogRule {
    val commentKey: String = "comment"
    val tagKey: String = "tag"
    val levelKey: String = "level"
    val positionKey: String = "position"
    val messageKey: String = "message"
    val extraKey: String = "extra"
    val tagIsKeyLog: String = "KEY"
    val tagNotKeyLog: String = "DEV"
  }

  object LogLevel {
    val debug: String = "D"
    val info: String = "I"
    val warn: String = "W"
    val error: String = "E"
  }

  object RuleTypes {
    val matchRule: String = "match"
    val sequenceRule: String = "sequence"
    val appearRule: String = "appear"
    val noAppearRule: String = "no_appear"
    val orderedRule: String = "ordered"
    val notRule: String = "not"
    val anyRule: String = "any"
    val allRule: String = "all"
  }
}