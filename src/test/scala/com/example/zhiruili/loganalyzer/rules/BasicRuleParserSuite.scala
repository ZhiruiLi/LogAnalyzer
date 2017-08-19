package com.example.zhiruili.loganalyzer.rules

import com.example.zhiruili.loganalyzer.logs.LvDebug
import org.scalatest.FunSuite
import play.api.libs.json.Json
import BasicRuleParserSuite._

import scala.util.{Failure, Success, Try}

class BasicRuleParserSuite extends FunSuite {

  test("Parse incomplete single log") {
    assertResult(Success(ExpectIncompleteRules.ruleSingleEmpty)) {
      BasicRuleParser.parseSingleLog(Json.parse(RuleStrings.ruleSingleEmpty))
    }
    assertResult(Success(ExpectIncompleteRules.ruleSingleSimple)) {
      BasicRuleParser.parseSingleLog(Json.parse(RuleStrings.ruleSingleSimple))
    }
    assertResult(Success(ExpectIncompleteRules.ruleSingleSimple)) {
      BasicRuleParser.parseRuleString(RuleStrings.ruleSingleSimple)
    }
  }

  test("Parse incomplete rule sequence") {
    assertResult(Success(ExpectIncompleteRules.ruleSequenceEmpty)) {
      BasicRuleParser.parseSequence(Json.parse(RuleStrings.ruleSequenceEmpty))
    }
    assertResult(Success(ExpectIncompleteRules.ruleSequenceSimple)) {
      BasicRuleParser.parseSequence(Json.parse(RuleStrings.ruleSequenceSimple))
    }
    assertResult(Success(ExpectIncompleteRules.ruleSequenceSimple)) {
      BasicRuleParser.parseRuleString(RuleStrings.ruleSequenceSimple)
    }
  }

  test("Parse incomplete rule appear") {
    assertResult(Success(ExpectIncompleteRules.ruleAppearName)) {
      BasicRuleParser.parseAppear(Json.parse(RuleStrings.ruleAppearName))
    }
    assertResult(Success(ExpectIncompleteRules.ruleAppearSimple)) {
      BasicRuleParser.parseAppear(Json.parse(RuleStrings.ruleAppearSimple))
    }
    assertResult(Success(ExpectIncompleteRules.ruleAppearSimple)) {
      BasicRuleParser.parseRuleString(RuleStrings.ruleAppearSimple)
    }
  }

  test("Parse incomplete rule no appear") {
    assertResult(Success(ExpectIncompleteRules.ruleNoAppearName)) {
      BasicRuleParser.parseNoAppear(Json.parse(RuleStrings.ruleNoAppearName))
    }
    assertResult(Success(ExpectIncompleteRules.ruleNoAppearSimple)) {
      BasicRuleParser.parseNoAppear(Json.parse(RuleStrings.ruleNoAppearSimple))
    }
    assertResult(Success(ExpectIncompleteRules.ruleNoAppearSimple)) {
      BasicRuleParser.parseRuleString(RuleStrings.ruleNoAppearSimple)
    }
  }

  test("Parse incomplete rule not") {
    assertResult(Success(ExpectIncompleteRules.ruleNotName)) {
      BasicRuleParser.parseNot(Json.parse(RuleStrings.ruleNotName))
    }
    assertResult(Success(ExpectIncompleteRules.ruleNotSimple)) {
      BasicRuleParser.parseNot(Json.parse(RuleStrings.ruleNotSimple))
    }
    assertResult(Success(ExpectIncompleteRules.ruleNotSimple)) {
      BasicRuleParser.parseRuleString(RuleStrings.ruleNotSimple)
    }
  }

  test("Parse incomplete rule match all") {
    assertResult(Success(ExpectIncompleteRules.ruleMatchAllEmpty)) {
      BasicRuleParser.parseMatchAll(Json.parse(RuleStrings.ruleMatchAllEmpty))
    }
    assertResult(Success(ExpectIncompleteRules.ruleMatchAllSimple)) {
      BasicRuleParser.parseMatchAll(Json.parse(RuleStrings.ruleMatchAllSimple))
    }
    assertResult(Success(ExpectIncompleteRules.ruleMatchAllSimple)) {
      BasicRuleParser.parseRuleString(RuleStrings.ruleMatchAllSimple)
    }
  }

  test("Parse incomplete rule match any") {
    assertResult(Success(ExpectIncompleteRules.ruleMatchAnyEmpty)) {
      BasicRuleParser.parseMatchAny(Json.parse(RuleStrings.ruleMatchAnyEmpty))
    }
    assertResult(Success(ExpectIncompleteRules.ruleMatchAnySimple)) {
      BasicRuleParser.parseMatchAny(Json.parse(RuleStrings.ruleMatchAnySimple))
    }
    assertResult(Success(ExpectIncompleteRules.ruleMatchAnySimple)) {
      BasicRuleParser.parseRuleString(RuleStrings.ruleMatchAnySimple)
    }
  }

  test("Fully initialize should replace all RuleNames to actual rules") {
    import ExpectIncompleteRules._
    def assertHelper(rule: Rule, incompleteRule: IncompleteRule): Unit = {
      import ExpectRules.fakeRuleGetter
      assertResult(rule) {
        incompleteRule.fullyInitialize(fakeRuleGetter).get
      }
    }
    assertHelper(ExpectRules.ruleSingleEmpty, ruleSingleEmpty)
    assertHelper(ExpectRules.ruleSingleSimple, ruleSingleSimple)
    assertHelper(ExpectRules.ruleSequenceEmpty, ruleSequenceEmpty)
    assertHelper(ExpectRules.ruleSequenceSimple, ruleSequenceSimple)
    assertHelper(ExpectRules.ruleAppearName, ruleAppearName)
    assertHelper(ExpectRules.ruleAppearSimple, ruleAppearSimple)
    assertHelper(ExpectRules.ruleNoAppearName, ruleNoAppearName)
    assertHelper(ExpectRules.ruleNoAppearSimple, ruleNoAppearSimple)
    assertHelper(ExpectRules.ruleMatchAllEmpty, ruleMatchAllEmpty)
    assertHelper(ExpectRules.ruleMatchAllSimple, ruleMatchAllSimple)
    assertHelper(ExpectRules.ruleMatchAnyEmpty, ruleMatchAnyEmpty)
    assertHelper(ExpectRules.ruleMatchAnySimple, ruleMatchAnySimple)
  }
}

object BasicRuleParserSuite {

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

    val ruleSingleEmpty: String =
      """
        |{
        |  "type": "match"
        |}
      """.stripMargin

    val ruleSequenceEmpty: String =
      """
        |{
        |  "type": "sequence",
        |  "rules": []
        |}
      """.stripMargin

    val ruleSequenceSimple: String =
      s"""
         |{
         |  "type": "sequence",
         |  "rules": [
         |    "abc",
         |    $ruleSingleEmpty,
         |    "def",
         |    $ruleSequenceEmpty
         |  ]
         |}
      """.stripMargin

    val ruleAppearName: String =
      """
        |{
        |  "type": "appear",
        |  "rule": "abc"
        |}
      """.stripMargin

    val ruleAppearSimple: String =
      s"""
         |{
         |  "type": "appear",
         |  "rule": $ruleSingleEmpty,
         |  "times": 3
         |}
     """.stripMargin

    val ruleNoAppearName: String =
      """
        |{
        |  "type": "no_appear",
        |  "rule": "abc"
        |}
      """.stripMargin

    val ruleNoAppearSimple: String =
      s"""
         |{
         |  "type": "no_appear",
         |  "rule": $ruleSingleEmpty
         |}
     """.stripMargin

    val ruleNotName: String =
      """
        |{
        |  "type": "not",
        |  "rule": "abc"
        |}
      """.stripMargin

    val ruleNotSimple: String =
      s"""
         |{
         |  "type": "not",
         |  "rule": $ruleSingleEmpty
         |}
     """.stripMargin

    val ruleMatchAllEmpty: String =
      s"""
         |{
         |  "type": "all",
         |  "rules": []
         |}
     """.stripMargin

    val ruleMatchAllSimple: String =
      s"""
         |{
         |  "type": "all",
         |  "rules": ["abc", $ruleSingleEmpty, "def", $ruleSequenceSimple]
         |}
     """.stripMargin

    val ruleMatchAnyEmpty: String =
      s"""
         |{
         |  "type": "any",
         |  "rules": []
         |}
     """.stripMargin

    val ruleMatchAnySimple: String =
      s"""
         |{
         |  "type": "any",
         |  "rules": ["abc", $ruleSingleEmpty, "def", $ruleSequenceSimple]
         |}
     """.stripMargin
  }

  object ExpectIncompleteRules {

    import BasicRuleParser.BasicIncompleteRules._

    val ruleSingleEmpty = IncompleteSingleLog(None, None, None, None, None, Map.empty)
    val ruleSingleSimple = IncompleteSingleLog(
      Some("hello comment"),
      Some(true),
      Some(LvDebug),
      Some("pos"),
      Some("msg"),
      Map("k1" -> "v1", "k2" -> "v2"))

    val ruleSequenceEmpty = IncompleteSequence(Nil)
    val ruleSequenceSimple =
      IncompleteSequence(List(RuleName("abc"), ruleSingleEmpty, RuleName("def"), ruleSequenceEmpty))

    val ruleAppearName = IncompleteAppear(RuleName("abc"), 1)
    val ruleAppearSimple = IncompleteAppear(ruleSingleEmpty, 3)

    val ruleNoAppearName = IncompleteNoAppear(RuleName("abc"))
    val ruleNoAppearSimple = IncompleteNoAppear(ruleSingleEmpty)

    val ruleNotName = IncompleteNot(RuleName("abc"))
    val ruleNotSimple = IncompleteNot(ruleSingleEmpty)

    val ruleMatchAllEmpty = IncompleteMatchAll(List())
    val ruleMatchAllSimple =
      IncompleteMatchAll(List(RuleName("abc"), ruleSingleEmpty, RuleName("def"), ruleSequenceSimple))

    val ruleMatchAnyEmpty = IncompleteMatchAny(List())
    val ruleMatchAnySimple =
      IncompleteMatchAny(List(RuleName("abc"), ruleSingleEmpty, RuleName("def"), ruleSequenceSimple))
  }

  object ExpectRules {

    import BasicRules._

    val ruleAbc = RuleSequence(Nil)
    val ruleDef = MatchAllRules(Nil)

    val ruleSingleEmpty = SingleLog(None, None, None, None, None, Map.empty)
    val ruleSingleSimple = SingleLog(
      Some("hello comment"),
      Some(true),
      Some(LvDebug),
      Some("pos"),
      Some("msg"),
      Map("k1" -> "v1", "k2" -> "v2"))

    val fakeRuleGetter: (String) => Try[Rule] = {
      case "abc" => Success(ruleAbc)
      case "def" => Success(ruleDef)
      case ruleName => Failure(new RuntimeException(s"No such rule named: $ruleName"))
    }

    val ruleSequenceEmpty = RuleSequence(List())
    val ruleSequenceSimple =
      RuleSequence(List(ruleAbc, ruleSingleEmpty, ruleDef, ruleSequenceEmpty))

    val ruleAppearName = RuleAppear(ruleAbc)
    val ruleAppearSimple = RuleAppear(ruleSingleEmpty, 3)

    val ruleNoAppearName = RuleNoAppear(ruleAbc)
    val ruleNoAppearSimple = RuleNoAppear(ruleSingleEmpty)

    val ruleMatchAllEmpty = MatchAllRules(Nil)
    val ruleMatchAllSimple =
      MatchAllRules(List(ruleAbc, ruleSingleEmpty, ruleDef, ruleSequenceSimple))

    val ruleMatchAnyEmpty = MatchAnyRule(Nil)
    val ruleMatchAnySimple =
      MatchAnyRule(List(ruleAbc, ruleSingleEmpty, ruleDef, ruleSequenceSimple))
  }
}
