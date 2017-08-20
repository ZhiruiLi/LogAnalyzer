package com.example.zhiruili.loganalyzer.analyzer.config

import com.example.zhiruili.loganalyzer.analyzer.config.AnalyzerConfig.{HelpInfo, HelpInfoBinding, Problem}
import org.scalatest.FunSuite

class DefaultConfigParserSuite extends FunSuite {

  test("Parse init json string") {
    assertResult(DefaultConfigParserSuite.config1) {
      DefaultConfigParser.parseConfigString(DefaultConfigParserSuite.initJson1).get
    }
    assertResult(DefaultConfigParserSuite.config2) {
      DefaultConfigParser.parseConfigString(DefaultConfigParserSuite.initJson2).get
    }
  }
}

object DefaultConfigParserSuite {

  val helpBindStr1: String =
    """
      |{
      |  "rule_name": "aaa",
      |  "help_message": "hello",
      |  "help_page": "www.xxx.com"
      |}
    """.stripMargin

  val helpInfo1 = HelpInfo("hello", Some("www.xxx.com"))
  val helpBind1 = HelpInfoBinding("aaa", helpInfo1)

  val helpBindStr2: String =
    """
      |{
      |  "rule_name": "bbb",
      |  "help_message": "world"
      |}
    """.stripMargin

  val helpInfo2 = HelpInfo("world", None)
  val helpBind2 = HelpInfoBinding("bbb", helpInfo2)

  val problemBindStr1: String =
    s"""
      |{
      |  "problem": 1,
      |  "name": "pb1",
      |  "help_bindings": [$helpBindStr1, $helpBindStr2]
      |}
     """.stripMargin

  val problemBindStr2: String =
    s"""
       |{
       |  "problem": 2,
       |  "name": "pb2",
       |  "help_bindings": [$helpBindStr2]
       |}
     """.stripMargin

  val problemBind1 = List(Problem(1, "pb1") -> List(helpBind1, helpBind2), Problem(2, "pb2") -> List(helpBind2))

  val initJson1: String =
    s"""
      |{
      |  "extend": "1.9.1",
      |  "problem_bindings": [$problemBindStr1, $problemBindStr2]
      |}
    """.stripMargin

  val config1 = ExtendConfig("1.9.1", RootConfig(problemBind1))

  val initJson2: String =
    s"""
       |{
       |  "problem_bindings": []
       |}
     """.stripMargin

  val config2 = RootConfig(Nil)
}
