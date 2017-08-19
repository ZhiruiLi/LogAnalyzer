package com.example.zhiruili.loganalyzer.rules

import org.scalatest.FunSuite
import BufferRuleLoaderSuite._
import RuleStrings._
import com.example.zhiruili.loganalyzer._
import com.example.zhiruili.loganalyzer.logs.LvDebug
import com.example.zhiruili.loganalyzer.rules.BasicRules.{RuleNoAppear, SingleLog}
import com.example.zhiruili.loganalyzer.rules.RuleLoader.{NoSuchRuleException, RecursiveLoadRuleException}

import scala.util.{Failure, Success, Try}

class BufferRuleLoaderSuite extends FunSuite {

  test("Load independent rules should success") {
    val loader = BufferRuleLoaderSuite
      .makeLoader(List("abc" -> ruleSingleSimpleKey, "def" -> ruleSingleSimpleKey))
    assertResult(Success(ExpectRules.ruleSingleSimple)) {
      loader("abc")
    }
    assertResult(Success(ExpectRules.ruleSingleSimple)) {
      loader("def")
    }
    assertResult(Success(ExpectRules.ruleSingleSimple)) {
      loader("abc")
    }
  }

  test("Load dependent rules without recursion should success") {
    val loader1 = BufferRuleLoaderSuite
      .makeLoader(List("abc" -> ruleSingleSimpleKey, "def" -> ruleNoAppearAbcKey))
    assertResult(Success(ExpectRules.ruleSingleSimple)) {
      loader1("abc")
    }
    assertResult(Success(ExpectRules.ruleNoAppearSingleSimple)) {
      loader1("def")
    }
    val loader2 = BufferRuleLoaderSuite
      .makeLoader(List("abc" -> ruleNoAppearDefKey, "def" -> ruleSingleSimpleKey))
    assertResult(Success(ExpectRules.ruleNoAppearSingleSimple)) {
      loader2("abc")
    }
    assertResult(Success(ExpectRules.ruleSingleSimple)) {
      loader2("def")
    }
  }

  test("Recursive loading should fail") {
    val loader = BufferRuleLoaderSuite
      .makeLoader(List("abc" -> ruleNoAppearDefKey, "def" -> ruleNoAppearAbcKey))
    assertThrows[RecursiveLoadRuleException](loader("abc").get)
    assertThrows[RecursiveLoadRuleException](loader("def").get)
  }

  test("Load nonexistent rule should fail") {
    val loader = BufferRuleLoaderSuite.makeLoader(List("abc" -> ruleSingleSimpleKey))
    assertThrows[NoSuchRuleException](loader("def").get)
  }
}

object BufferRuleLoaderSuite {

  val ruleSingleSimpleKey = "ruleSingleSimple"
  val ruleNoAppearAbcKey = "ruleNoAppearAbc"
  val ruleNoAppearDefKey = "ruleNoAppearDef"

  class TestLoader(val unloadedRules: Map[String, String]) extends RuleLoader {

    override def parser: RuleParser = BasicRuleParser

    val ruleStringMap = Map(
      ruleSingleSimpleKey -> ruleSingleSimple,
      ruleNoAppearAbcKey -> ruleNoAppearAbc,
      ruleNoAppearDefKey -> ruleNoAppearDef)

    def apply(ruleName: String): Try[Rule] = loadRule(ILiveSdk, PlatformAndroid, "")(ruleName)

    override def loadIncompleteRule
    (sdk: Sdk, platform: Platform, version: Version)(ruleName: String): Try[IncompleteRule] = {
      unloadedRules.get(ruleName).flatMap(ruleStringMap.get) match {
        case None => Failure(NoSuchRuleException(sdk, platform, version, ruleName))
        case Some(str) => parser.parseRuleString(str)
      }
    }
  }

  def makeLoader(ruleSources: List[(String, String)]): TestLoader = new TestLoader(ruleSources.toMap)

  object RuleStrings {

    val ruleSingleSimple: String =
      """
        |{
        |  "type": "match",
        |  "comment": "hello comment",
        |  "tag": "KEY",
        |  "level": "D",
        |  "position": "pos",
        |  "message": "msg",
        |  "extra": { "k1": "v1", "k2": "v2" }
        |}
      """.stripMargin

    val ruleNoAppearAbc: String =
      """
        |{
        |  "type": "no_appear",
        |  "rule": "abc"
        |}
      """.stripMargin

    val ruleNoAppearDef: String =
      """
        |{
        |  "type": "no_appear",
        |  "rule": "def"
        |}
      """.stripMargin
  }

  object ExpectRules {

    val ruleSingleSimple = SingleLog(
      Some("hello comment"),
      Some(true),
      Some(LvDebug),
      Some("pos"),
      Some("msg"),
      Map("k1" -> "v1", "k2" -> "v2"))

    val ruleNoAppearSingleSimple = RuleNoAppear(ruleSingleSimple)
  }
}

