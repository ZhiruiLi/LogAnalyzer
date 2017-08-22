package com.example.zhiruili.loganalyzer.rules

import java.text.SimpleDateFormat
import java.util.Date

import com.example.zhiruili.loganalyzer.logs.{LegalLog, LvDebug, LvInfo}
import com.example.zhiruili.loganalyzer.rules.BasicRulesSuite.TestConstants._
import org.scalatest.{FunSuite, Tag}

class BasicRulesSuite extends FunSuite {

  val singleLogTag = Tag(classOf[BasicRules.SingleLog].getSimpleName)

  test("Rule SingleLog should match the first legal log desired", singleLogTag) {
    assertResult(MatchSuccess(List(logSimple), Nil, Nil)) {
      singleLogRuleAllNone.matchLogItems(List(logSimple))
    }
    assertResult(MatchSuccess(List(logSimple), Nil, List(logSimple, logSimple))) {
      singleLogRuleAllNone.matchLogItems(List(logSimple, logSimple, logSimple))
    }
    assertResult(MatchSuccess(List(logSimple), Nil, List(logNotKey))) {
      singleLogRuleAllNone.matchLogItems(List(logSimple, logNotKey))
    }
    assertResult(MatchSuccess(List(logNotKey), Nil, List(logSimple))) {
      singleLogRuleAllNone.matchLogItems(List(logNotKey, logSimple))
    }
    assertResult(MatchSuccess(List(logSimple), Nil, Nil)) {
      singleLogRuleSimple.matchLogItems(List(logSimple))
    }
  }

  test("Rule SingleLog should match regex", singleLogTag) {
    assertResult(MatchSuccess(List(logSimple), Nil, Nil)) {
      singleLogRuleRegex.matchLogItems(List(logSimple))
    }
    assertResult(MatchSuccess(List(logDiffPos), Nil, Nil)) {
      singleLogRuleRegex.matchLogItems(List(logDiffPos))
    }
    assertResult(MatchFailure(List(logDiffPosLetter), Nil, Nil, Nil)) {
      singleLogRuleRegex.matchLogItems(List(logDiffPosLetter))
    }
  }

  test("Test match different parts of LegalLog", singleLogTag) {
    assertResult(MatchSuccess(List(logDiffLevel), Nil, Nil)) {
      singleLogRuleLevel.matchLogItems(List(logDiffLevel))
    }
    assertResult(MatchFailure(List(logSimple), Nil, Nil, Nil)) {
      singleLogRuleLevel.matchLogItems(List(logSimple))
    }
    assertResult(MatchSuccess(List(logDiffExt), Nil, Nil)) {
      singleLogRuleExt1.matchLogItems(List(logDiffExt))
    }
    assertResult(MatchSuccess(List(logDiffExt), Nil, Nil)) {
      singleLogRuleExt1.matchLogItems(List(logDiffExt))
    }
    assertResult(MatchFailure(List(logDiffExt), Nil, Nil, Nil)) {
      singleLogRuleExt2.matchLogItems(List(logDiffExt))
    }
    assertResult(MatchSuccess(List(logDiffExtMore), Nil, Nil)) {
      singleLogRuleExt2.matchLogItems(List(logDiffExtMore))
    }
    assertResult(MatchFailure(List(logDiffExt), Nil, Nil, Nil)) {
      singleLogRuleExt3.matchLogItems(List(logDiffExt))
    }
    assertResult(MatchSuccess(List(logDiffExtMore), Nil, Nil)) {
      singleLogRuleExt3.matchLogItems(List(logDiffExtMore))
    }
  }

  test("Rule SingleLog should only match the head log", singleLogTag) {
    assertResult(MatchSuccess(List(logSimple), Nil, List(logNotKey))) {
      singleLogRuleSimple.matchLogItems(List(logSimple, logNotKey))
    }
    assertResult(MatchFailure(List(logNotKey), Nil, Nil, List(logSimple))) {
      singleLogRuleSimple.matchLogItems(List(logNotKey, logSimple))
    }
    assertResult(MatchFailure(Nil, Nil, Nil, Nil)) {
      singleLogRuleSimple.matchLogItems(Nil)
    }
  }

  val appearTag = Tag(classOf[BasicRules.RuleAppear].getSimpleName)

  test("Rule RuleAppear should success only when the rule appears at least the given times", appearTag) {
    assertResult(MatchSuccess(List(logDiffLevel, logDiffLevel, logDiffLevel), Nil, Nil)) {
      appearRuleLevel3Times.matchLogItems(List(logDiffLevel, logDiffLevel, logDiffLevel))
    }
    assertResult(MatchSuccess(List(logDiffLevel, logDiffLevel, logDiffLevel), List(logSimple, logSimple), List(logDiffLevel))) {
      appearRuleLevel3Times.matchLogItems(List(logSimple, logDiffLevel, logSimple, logDiffLevel, logDiffLevel, logDiffLevel))
    }
    assertResult(MatchFailure(Nil, List(logDiffLevel, logDiffLevel), Nil, Nil)) {
      appearRuleLevel3Times.matchLogItems(List(logDiffLevel, logDiffLevel))
    }
    assertResult(MatchFailure(Nil, List(logDiffLevel, logDiffLevel), List(logSimple, logDiffExt), Nil)) {
      appearRuleLevel3Times.matchLogItems(List(logSimple, logDiffLevel, logDiffLevel, logDiffExt))
    }
  }

  val noAppearTag = Tag(classOf[BasicRules.RuleNoAppear].getSimpleName)

  test("Rule NoAppearRule should success if the rule not match anywhere", noAppearTag) {
    assertResult(MatchSuccess(Nil, List(logSimple), Nil)) {
      noAppearRuleLv.matchLogItems(List(logSimple))
    }
    assertResult(MatchSuccess(Nil, List(logSimple, logDiffExt), Nil)) {
      noAppearRuleLv.matchLogItems(List(logSimple, logDiffExt))
    }
    assertResult(MatchFailure(List(logDiffLevel), Nil, List(logSimple, logDiffExt), List(logDiffLevel))) {
      noAppearRuleLv.matchLogItems(List(logSimple, logDiffExt, logDiffLevel, logDiffLevel))
    }
  }

  val notTag = Tag(classOf[BasicRules.RuleNoAppear].getSimpleName)

  test("Rule NotRule should negate the matching result of a rule", notTag) {
    assertResult(MatchSuccess(List(logSimple), Nil, Nil)) {
      notRuleLv.matchLogItems(List(logSimple))
    }
    assertResult(MatchSuccess(List(logSimple), Nil, List(logDiffExt))) {
      notRuleLv.matchLogItems(List(logSimple, logDiffExt))
    }
    assertResult(MatchSuccess(List(logSimple), Nil, List(logDiffExt, logDiffLevel))) {
      notRuleLv.matchLogItems(List(logSimple, logDiffExt, logDiffLevel))
    }
    assertResult(MatchFailure(List(logDiffLevel), Nil, Nil, List(logSimple, logDiffExt))) {
      notRuleLv.matchLogItems(List(logDiffLevel, logSimple, logDiffExt))
    }
  }

  val matchAllTag = Tag(classOf[BasicRules.MatchAllRules].getSimpleName)

  test("Rule MatchAllRules should only success when all rules are matched", matchAllTag) {
    assertResult(MatchSuccess(List(logSimple, logDiffExt), List(logSimple), List(logSimple, logDiffExt))) {
      matchAllRuleSimAndExt1.matchLogItems(List(logSimple, logDiffExt))
    }
    assertResult(MatchFailure(List(), List(logSimple), List(logSimple), List(logSimple))) {
      matchAllRuleSimAndExt1.matchLogItems(List(logSimple))
    }
    assertResult(MatchSuccess(List(logDiffExt, logDiffExt), Nil, List(logDiffExt))) {
      matchAllRuleSimAndExt1.matchLogItems(List(logDiffExt))
    }
  }

  val matchAnyTag = Tag(classOf[BasicRules.MatchAnyRule].getSimpleName)

  test("Rule MatchAnyRule should success when any rule in list is matched", matchAnyTag) {
    assertResult(MatchSuccess(List(logSimple), Nil, List(logDiffExt))) {
      matchAnyRuleSimAndExt1.matchLogItems(List(logSimple, logDiffExt))
    }
    assertResult(MatchSuccess(List(logDiffExt), Nil, List(logSimple))) {
      matchAnyRuleSimAndExt1.matchLogItems(List(logDiffExt, logSimple))
    }
    assertResult(MatchSuccess(List(logSimple), List(logDiffLevel), Nil)) {
      matchAnyRuleSimAndExt1.matchLogItems(List(logDiffLevel, logSimple))
    }
    assertResult(MatchFailure(Nil, Nil, List(logDiffLevel, logDiffLevel), List(logDiffLevel))) {
      matchAnyRuleSimAndExt1.matchLogItems(List(logDiffLevel))
    }
    assertResult(MatchSuccess(List(logSimple), Nil, List(logDiffExt))) {
      matchAnyRuleSimAndExt1FromHead.matchLogItems(List(logSimple, logDiffExt))
    }
    assertResult(MatchSuccess(List(logDiffExt), Nil, List(logSimple))) {
      matchAnyRuleSimAndExt1FromHead.matchLogItems(List(logDiffExt, logSimple))
    }
    assertResult(MatchFailure(List(logDiffLevel, logDiffLevel), Nil, Nil, List(logDiffLevel, logSimple, logDiffExt))) {
      matchAnyRuleSimAndExt1FromHead.matchLogItems(List(logDiffLevel, logSimple, logDiffExt))
    }
  }

  val seqRulesTag = Tag(classOf[BasicRules.RuleSequence].getSimpleName)

  test("Rule RuleSequence should success only all rules are matched in order", seqRulesTag) {
    assertResult(MatchSuccess(List(logSimple, logDiffLevel), Nil, List(logSimple))) {
      sequenceRuleSimAndLv.matchLogItems(List(logSimple, logDiffLevel, logSimple))
    }
  }

  test("The matched log of RuleSequence should arrange closely") {
    assertResult(MatchFailure(List(logSimple), List(logSimple), Nil, List(logDiffLevel))) {
      sequenceRuleSimAndLv.matchLogItems(List(logSimple, logSimple, logDiffLevel))
    }
    assertResult(MatchFailure(Nil, List(logSimple), Nil, Nil)) {
      sequenceRuleSimAndLv.matchLogItems(List(logSimple))
    }
  }

  val orderedRulesTag = Tag(classOf[BasicRules.RuleOrdered].getSimpleName)

  test("Rule RuleOrdered should success only all rules are matched in order", orderedRulesTag) {
    assertResult(MatchSuccess(List(logDiffExt, logDiffLevel), Nil, List(logDiffExt))) {
      orderRuleExt1AndLv.matchLogItems(List(logDiffExt, logDiffLevel, logDiffExt))
    }
    assertResult(MatchSuccess(List(logDiffExt, logDiffLevel), List(logDiffExt), Nil)) {
      orderRuleExt1AndLv.matchLogItems(List(logDiffExt, logDiffExt, logDiffLevel))
    }
    assertResult(MatchFailure(List(logSimple), Nil, Nil, List(logDiffExt, logDiffLevel))) {
      orderRuleExt1AndLv.matchLogItems(List(logSimple, logDiffExt, logDiffLevel))
    }
    assertResult(MatchFailure(List(logDiffLevel), Nil, Nil, List(logDiffExt))) {
      orderRuleExt1AndLv.matchLogItems(List(logDiffLevel, logDiffExt))
    }
  }
}

object BasicRulesSuite {

  object TestConstants {

    import BasicRules._

    val singleLogRuleAllNone = SingleLog(None, None, None, None, None, Map.empty)
    val singleLogRuleSimple = SingleLog(None, Some(true), Some(LvDebug), Some("pos01"), Some("msg01"), Map.empty)
    val singleLogRuleRegex = SingleLog(None, None, None, Some("pos0\\d"), None, Map.empty)
    val singleLogRuleLevel = SingleLog(None, None, Some(LvInfo), None, None, Map.empty)
    val singleLogRuleExt1 = SingleLog(None, None, None, None, None, Map("a" -> ".*"))
    val singleLogRuleExt2 = SingleLog(None, None, None, None, None, Map("a" -> "\\w+\\d+\\w+"))
    val singleLogRuleExt3 = SingleLog(None, None, None, None, None, Map("a" -> ".*", "b" -> ".*"))

    val appearRuleLevel3Times = RuleAppear(singleLogRuleLevel, 3)
    val appearRuleSimple = RuleAppear(singleLogRuleSimple)
    val appearRuleExt1 = RuleAppear(singleLogRuleExt1)

    val noAppearRuleLv = RuleNoAppear(singleLogRuleLevel)

    val notRuleLv = RuleNot(singleLogRuleLevel)

    val matchAllRuleSimAndExt1 = MatchAllRules(List(appearRuleSimple, appearRuleExt1))

    val matchAnyRuleSimAndExt1 = MatchAnyRule(List(appearRuleSimple, appearRuleExt1))
    val matchAnyRuleSimAndExt1FromHead = MatchAnyRule(List(singleLogRuleSimple, singleLogRuleExt1))

    val sequenceRuleSimAndLv = RuleSequence(List(singleLogRuleSimple, singleLogRuleLevel))

    val orderRuleExt1AndLv = RuleOrdered(List(singleLogRuleExt1, singleLogRuleLevel))

    val date: Date = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse("2012-01-15 12:21:01")

    val logSimple = new LegalLog("", date, isKeyLog = true, LvDebug, "pos01", "msg01", Map.empty) {
      override def toString: String = "logSimple"
    }
    val logNotKey = new LegalLog("", date, isKeyLog = false, LvDebug, "pos01", "msg01", Map.empty) {
      override def toString: String = "logNotKey"
    }
    val logDiffPos = new LegalLog("", date, isKeyLog = true, LvDebug, "pos03", "msg01", Map.empty) {
      override def toString: String = "logDiffPos"
    }
    val logDiffPosLetter = new LegalLog("", date, isKeyLog = true, LvDebug, "pos0x", "msg01", Map.empty) {
      override def toString: String = "logDiffPosLetter"
    }
    val logDiffLevel = new LegalLog("", date, isKeyLog = true, LvInfo, "pos01", "msg01", Map.empty) {
      override def toString: String = "logDiffLevel"
    }
    val logDiffExt = new LegalLog("", date, isKeyLog = true, LvDebug, "pos01", "msg01", Map("a" -> "bcd")) {
      override def toString: String = "logDiffExt"
    }
    val logDiffExtMore = new LegalLog("", date, isKeyLog = true, LvDebug, "pos01", "msg01", Map("a" -> "x12x", "b" -> "0000", "c" -> "yyyy")) {
      override def toString: String = "logDiffExtMore"
    }
  }
}
