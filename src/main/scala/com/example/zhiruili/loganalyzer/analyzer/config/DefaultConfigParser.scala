package com.example.zhiruili.loganalyzer.analyzer.config

import com.example.zhiruili.loganalyzer.analyzer.config.AnalyzerConfig.{HelpInfo, HelpInfoBinding, Problem}
import play.api.libs.json.{JsArray, JsValue, Json}

import scala.util.Try

/**
  * 默认配置文件解析器，规则用 json 格式表示：
  * {
  *   "extend": version,                // 可选，string，继承自其他版本的规则
  *   "problem_bindings": [             // 必选，json 数组，绑定问题和对应的匹配规则
  *     {
  *       "problem": problem_code,      // 必选，问题代号
  *       "name": problem_name,         // 必选，问题名，用于给用户提示
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
object DefaultConfigParser extends ConfigParser {

  object Keys {

    val extend: String = "extend"
    val problemBindings: String = "problem_bindings"

    val problemCode: String = "problem"
    val problemName: String = "name"
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

  def parseProblemBindings(jsValue: JsValue): Try[List[(Problem, List[HelpInfoBinding])]] = {
    parseJsonArray(jsValue, Keys.problemBindings)
      .flatMap(rawBinds => rawBinds
        .foldRight(Try(List.empty[(Problem, List[HelpInfoBinding])])) {
          case (rawBind, b) => for {
            lst <- b
            binding <- parseProblemBinding(rawBind)
          } yield binding :: lst
        }
      )
  }

  def parseProblemBinding(jsValue: JsValue): Try[(Problem, List[HelpInfoBinding])] = for {
    probCode <- Try((jsValue \ Keys.problemCode).as[Int])
    probName <- Try((jsValue \ Keys.problemName).as[String])
    helpBinds <- parseHelpInfoBindings(jsValue)
  } yield (Problem(probCode, probName), helpBinds)

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
