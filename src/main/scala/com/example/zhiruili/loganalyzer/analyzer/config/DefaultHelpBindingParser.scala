package com.example.zhiruili.loganalyzer.analyzer.config

import com.example.zhiruili.loganalyzer.analyzer.{AnalyzerConfig, ExtendConfig, RootConfig}
import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.{HelpInfo, HelpInfoBinding, ProblemTag}
import play.api.libs.json.{JsArray, JsValue, Json}

import scala.util.Try

/**
  * 默认配置文件解析器，规则用 json 格式表示：
  * {
  *   "extend": version,                // 可选，string，继承自其他版本的规则
  *   "problem_bindings": [             // 必选，json 数组，绑定问题和对应的匹配规则
  *     {
  *       "problem": problem_tag,       // 必选，问题标签
  *       "help_bindings": [            // 必选，绑定匹配规则和帮助信息
  *         {
  *           "rule_name": rule_name,   // 必选，规则名
  *           "help_message": message,  // 必选，规则匹配成功后的帮助信息
  *           "help_page": page_url     // 可选，规则匹配成功后的帮助页面地址
  *         },
  *         {
  *           "rule_name": rule_name,
  *           "help_message": message
  *         }
  *       ]
  *     },
  *     {
  *       "problem": problem_code,
  *       "help_bindings": [
  *         {
  *           "rule_name": rule_name,
  *           "help_message": message
  *         }
  *       ]
  *     }
  *   ]
  * }
  */
object DefaultHelpBindingParser extends HelpBindingParser {

  object Keys {

    val extend: String = "extend"
    val problemBindings: String = "problem_bindings"

    val problemTag: String = "problem"
    val helpBindings: String = "help_bindings"

    val ruleName: String = "rule_name"
    val helpMessage: String = "help_message"
    val helpPage: String = "help_page"
  }

  override def parseConfigString(str: String): Try[AnalyzerConfig] = for {
    jsValue <- Try(Json.parse(str))
    bindings <- parseProblemBindings(jsValue)
    optExtend = (jsValue \ Keys.extend).asOpt[String]
    baseConfig = RootConfig(bindings)
  } yield optExtend.map(ver => ExtendConfig(ver, baseConfig)).getOrElse(baseConfig)

  def parseJsonArray(jsValue: JsValue, key: String): Try[List[JsValue]] = {
    Try((jsValue \ key).as[JsArray]).map(_.value.toList)
  }

  def parseProblemBindings(jsValue: JsValue): Try[List[(ProblemTag, List[HelpInfoBinding])]] = {
    parseJsonArray(jsValue, Keys.problemBindings)
      .flatMap(rawBinds => rawBinds
        .foldRight(Try(List.empty[(ProblemTag, List[HelpInfoBinding])])) {
          case (rawBind, b) => for {
            lst <- b
            binding <- parseProblemBinding(rawBind)
          } yield binding :: lst
        }
      )
  }

  def parseProblemBinding(jsValue: JsValue): Try[(ProblemTag, List[HelpInfoBinding])] = for {
    probTag <- Try((jsValue \ Keys.problemTag).as[String])
    helpBinds <- parseHelpInfoBindings(jsValue)
  } yield (ProblemTag(probTag), helpBinds)

  def parseHelpInfoBindings(jsValue: JsValue): Try[List[HelpInfoBinding]] = {
    parseJsonArray(jsValue, Keys.helpBindings)
      .flatMap(rawBinds => rawBinds
        .foldRight(Try(List.empty[HelpInfoBinding])) {
          case (rawBind, b) => for {
            lst <- b
            binding <- parseHelpInfoBinding(rawBind)
          } yield binding::lst
        }
      )
  }

  def parseHelpInfoBinding(jsValue: JsValue): Try[HelpInfoBinding] = for {
    ruleName <- Try((jsValue \ Keys.ruleName).as[String])
    helpMessage <- Try((jsValue \ Keys.helpMessage).as[String])
    helpPage = (jsValue \ Keys.helpPage).asOpt[String]
  } yield HelpInfoBinding(ruleName, HelpInfo(helpMessage, helpPage))
}
