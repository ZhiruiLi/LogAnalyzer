package com.example.zhiruili.loganalyzer.rules

import com.example.zhiruili.loganalyzer.logs._
import com.example.zhiruili.loganalyzer.rules.BasicRuleParser.BasicIncompleteRules.{IncompleteMatchAll, IncompleteMatchAny, IncompleteOrdered, IncompleteSequence}
import play.api.libs.json._
import BasicRuleContract._

import scala.util.{Failure, Success, Try}

object BasicRuleParser extends RuleParser {

  /**
    * 基础未完全解析的规则，与 BasicRules 一一对应
    */
  object BasicIncompleteRules {

    case class IncompleteSingleLog(optComment: Option[String], optIsKeyLog: Option[Boolean],
                                   optLevel: Option[LogLevel], optPosRegex: Option[String],
                                   optMsgRegex: Option[String], extMsgRegex: Map[String, String]) extends IncompleteRule {

      override def fullyInitialize(findRuleByName: (String) => Try[Rule]): Try[Rule] = Try {
        BasicRules.SingleLog(optComment, optIsKeyLog, optLevel, optPosRegex, optMsgRegex, extMsgRegex)
      }
    }

    def initializeAll(incompleteRules: List[IncompleteRule], findRuleByName: String => Try[Rule])
                     (consumer: List[Rule] => Rule): Try[Rule] = {

      incompleteRules.foldRight(Try(List.empty[Rule])) {
        case (incompleteRule, tryRules) => for {
          lst <- tryRules
          item <- incompleteRule.fullyInitialize(findRuleByName)
        } yield item::lst
      }.map(consumer)
    }

    case class IncompleteSequence(incomRules: List[IncompleteRule]) extends IncompleteRule {

      override def fullyInitialize(findRuleByName: (String) => Try[Rule]): Try[Rule] = {
        initializeAll(incomRules, findRuleByName)(BasicRules.RuleSequence)
      }
    }

    case class IncompleteOrdered(incomRules: List[IncompleteRule]) extends IncompleteRule {

      override def fullyInitialize(findRuleByName: (String) => Try[Rule]): Try[Rule] = {
        initializeAll(incomRules, findRuleByName)(BasicRules.RuleOrdered)
      }
    }

    case class IncompleteAppear(incomRule: IncompleteRule, times: Int) extends IncompleteRule {

      override def fullyInitialize(findRuleByName: (String) => Try[Rule]): Try[Rule] = {
        incomRule.fullyInitialize(findRuleByName).map(rule => BasicRules.RuleAppear(rule, times))
      }
    }

    case class IncompleteNoAppear(incomRule: IncompleteRule) extends IncompleteRule {

      override def fullyInitialize(findRuleByName: (String) => Try[Rule]): Try[Rule] = {
        incomRule.fullyInitialize(findRuleByName).map(BasicRules.RuleNoAppear)
      }
    }

    case class IncompleteNot(incomRule: IncompleteRule) extends IncompleteRule {

      override def fullyInitialize(findRuleByName: (String) => Try[Rule]): Try[Rule] = {
        incomRule.fullyInitialize(findRuleByName).map(BasicRules.RuleNot)
      }
    }

    case class IncompleteMatchAny(incomRules: List[IncompleteRule]) extends IncompleteRule {

      override def fullyInitialize(findRuleByName: (String) => Try[Rule]): Try[Rule] = {
        initializeAll(incomRules, findRuleByName)(BasicRules.MatchAnyRule)
      }
    }

    case class IncompleteMatchAll(incomRules: List[IncompleteRule]) extends IncompleteRule {

      override def fullyInitialize(findRuleByName: (String) => Try[Rule]): Try[Rule] = {
        initializeAll(incomRules, findRuleByName)(BasicRules.MatchAllRules)
      }
    }
  }

  private val logLevelMap = Map(
    LogLevel.debug -> LvDebug,
    LogLevel.info -> LvInfo,
    LogLevel.warn -> LvWarn,
    LogLevel.error -> LvError
  )

  /**
    * 单条日志匹配，格式：
    * {
    *   "type": "match",          // 类型标记
    *   "comment": "comment"      // 可选，备注名
    *   "tag": "tag_string",      // 可选，是否主线日志标签
    *   "level": "lv_string",     // 可选，日志等级
    *   "position": "pos_string", // 可选，日志打印位置
    *   "message": "msg_string",  // 可选，日志信息
    *   "extra": {                // 可选，额外信息键值对
    *     "key01": "val01",
    *     "key02": "val02"
    *   }
    * }
    */
  val parseSingleLog: JsValue => Try[IncompleteRule] = json => {
    val optComment = (json \ SingleLogRule.commentKey).asOpt[String]
    val optIsKey = (json \ SingleLogRule.tagKey).asOpt[String].flatMap { tag =>
      if (tag == SingleLogRule.tagIsKeyLog) Some(true)
      else if (tag == SingleLogRule.tagNotKeyLog) Some(false)
      else None
    }
    val optLv = for {
      lvStr <- (json \ SingleLogRule.levelKey).asOpt[String]
      level <- logLevelMap.get(lvStr)
    } yield level
    val optPos = (json \ SingleLogRule.positionKey).asOpt[String]
    val optMsg = (json \ SingleLogRule.messageKey).asOpt[String]
    val ext =
      (json \ SingleLogRule.extraKey).
        asOpt[JsObject].
        map(_.fields.toMap).
        getOrElse(Map.empty).
        mapValues(_.asOpt[String].getOrElse(".*"))
    Success(BasicIncompleteRules.IncompleteSingleLog(optComment, optIsKey, optLv, optPos, optMsg, ext))
  }

  /**
    * 匹配规则序列，被匹配日志需紧密排列，格式：
    * {
    *   "type": "sequence",       // 类型标记
    *   "rules": [                // 日志序列数组
    *     "rule01",               // 日志，可以是日志名
    *     {                       // 日志，可以是 inline 日志
    *       "type": "match",
    *       "message": "xxx"
    *     }
    *   ]
    * }
    */
  val parseSequence: JsValue => Try[IncompleteRule] = json => {
    parseIncompleteRules(json).map(IncompleteSequence)
  }

  /**
    * 匹配规则序列，被匹配日志不需紧密排列，格式类似 sequence
    */
  val parseOrdered: JsValue => Try[IncompleteRule] = json => {
    parseIncompleteRules(json).map(IncompleteOrdered)
  }

  /**
    * 规则在任意位置出现多次，格式：
    * {
    *   "type": "appear",         // 类型标记
    *   "times" 3,                // 可选，匹配次数，默认为 1
    *   "rule": rule              // 规则名或 inline 规则
    * }
    */
  val parseAppear: JsValue => Try[IncompleteRule] = json => {
    val times = (json \ AppearRule.timesKey).asOpt[Int].getOrElse(AppearRule.defaultTimes)
    parseIncompleteRule(json)
      .map(rule => BasicIncompleteRules.IncompleteAppear(rule, times))
  }

  /**
    * 不出现某条规则，格式
    * {
    *   "type": "no_appear",    // 类型标记
    *   "rule": rule            // 规则名或 inline 规则
    * }
    */
  val parseNoAppear: JsValue => Try[IncompleteRule] = json => {
    parseIncompleteRule(json)
      .map(rule => BasicIncompleteRules.IncompleteNoAppear(rule))
  }

  /**
    * 某条规则匹配失败，格式类似 no_appear
    */
  val parseNot: JsValue => Try[IncompleteRule] = json => {
    parseIncompleteRule(json)
      .map(rule => BasicIncompleteRules.IncompleteNot(rule))
  }

  /**
    * 匹配任意一条规则，格式类似 sequence
    */
  val parseMatchAny: JsValue => Try[IncompleteRule] = json => {
    parseIncompleteRules(json).map(IncompleteMatchAny)
  }

  /**
    * 匹配全部规则，格式类似 sequence
    */
  val parseMatchAll: JsValue => Try[IncompleteRule] = json => {
    parseIncompleteRules(json).map(IncompleteMatchAll)
  }

  val basicRuleParserMap = Map(
    RuleTypes.matchRule -> parseSingleLog,
    RuleTypes.sequenceRule -> parseSequence,
    RuleTypes.appearRule -> parseAppear,
    RuleTypes.noAppearRule -> parseNoAppear,
    RuleTypes.orderedRule -> parseOrdered,
    RuleTypes.notRule -> parseNot,
    RuleTypes.anyRule -> parseMatchAny,
    RuleTypes.allRule -> parseMatchAll
  )

  def parseIncompleteRule(json: JsValue): Try[IncompleteRule] = {
    Try((json \ General.singleRuleKey).as[JsValue]).flatMap { jsValue =>
      jsValue.asOpt[String] match {
        case Some(name) => Success(RuleName(name))
        case None => parseRuleString(jsValue.toString)
      }
    }
  }

  def parseIncompleteRules(json: JsValue): Try[List[IncompleteRule]] = {
    Try((json \ General.multiRulesKey).as[JsArray]).flatMap { jsArray =>
      jsArray
        .value
        .map(item => item.asOpt[String] match {
          case Some(name) => Success(RuleName(name))
          case None => parseRuleString(item.toString)
        })
        .foldRight(Try(List.empty[IncompleteRule])) {
          case (_, err@Failure(_)) => err
          case (Failure(thw), _) => Failure(thw)
          case (Success(rule), Success(rules)) => Success(rule::rules)
        }
    }
  }

  override def parseRuleString(str: String): Try[IncompleteRule] = for {
    jsValue <- Try(Json.parse(str))
    ruleName <- Try((jsValue \ General.ruleTypeKey).as[String])
    ruleParser <- getBasicRuleParser(ruleName)
    res <- ruleParser(jsValue)
  } yield res

  def getBasicRuleParser(name: String): Try[JsValue => Try[IncompleteRule]] = {
    basicRuleParserMap.get(name) match {
      case None => Failure(UnknownRuleException(name))
      case Some(parser) => Success(parser)
    }
  }

  case class UnknownRuleException(ruleName: String) extends RuntimeException(s"Unknown rule: $ruleName")
}
