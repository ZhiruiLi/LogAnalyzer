package com.example.zhiruili.loganalyzer.logs

import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.FunSuite

import UtilsSuite._

class UtilsSuite extends FunSuite {

  test("Test time filter") {
    assertResult(legalLogs.tail.init) {
      Utils.timeFilter(testTimes(1), testTimes(3))(legalLogs)
    }
    val logsKeep = List(unknownLogs(1), legalLogs(1), legalLogs(2), legalLogs(3), unknownLogs(2))
    assertResult(logsKeep) {
      Utils.timeFilter(testTimes(1), testTimes(3))(
        List(unknownLogs.head, legalLogs.head) ++ logsKeep ++ List(legalLogs(4), unknownLogs(3)))
    }
  }

  test("Filtering EquivocalLogs with time should behave exactly the same as filtering LegalLogs") {
    assertResult(equivocalLogsWithTime.tail.init) {
      Utils.timeFilter(testTimes(1), testTimes(3))(equivocalLogsWithTime)
    }
    val logsKeep = List(unknownLogs(1), equivocalLogsWithTime(1), equivocalLogsWithTime(2), equivocalLogsWithTime(3), unknownLogs(2))
    assertResult(logsKeep) {
      Utils.timeFilter(testTimes(1), testTimes(3))(
        List(unknownLogs.head, equivocalLogsWithTime.head) ++ logsKeep ++ List(equivocalLogsWithTime(4), unknownLogs(3)))
    }
  }

  test("Filtering EquivocalLogs without time should behave exactly the same as filtering UnknownLogs") {
    val logsKeep1 = List(equivocalLogsWithoutTime(1), legalLogs(1), legalLogs(2), legalLogs(3), equivocalLogsWithoutTime(2))
    assertResult(logsKeep1) {
      Utils.timeFilter(testTimes(1), testTimes(3))(
        List(equivocalLogsWithoutTime.head, legalLogs.head) ++ logsKeep1 ++ List(legalLogs(4), equivocalLogsWithoutTime(3)))
    }
    val logsKeep2 = List(
      equivocalLogsWithoutTime(1), equivocalLogsWithTime(1), equivocalLogsWithTime(2), equivocalLogsWithTime(3), equivocalLogsWithoutTime(2))
    assertResult(logsKeep2) {
      Utils.timeFilter(testTimes(1), testTimes(3))(
        List(equivocalLogsWithoutTime.head, equivocalLogsWithTime.head) ++ logsKeep2 ++ List(equivocalLogsWithTime(4), equivocalLogsWithoutTime(3)))
    }
  }

  test("Time filter should keep all logs if given time is None") {
    assertResult(Utils.timeFilter(Some(0L), Some(Long.MaxValue))(legalLogs)) {
      Utils.timeFilter(None, None)(legalLogs)
    }
    assertResult(Utils.timeFilter(Some(0L), Some(Long.MaxValue))(unknownLogs ++ legalLogs ++ unknownLogs)) {
      Utils.timeFilter(None, None)(unknownLogs ++ legalLogs ++ unknownLogs)
    }
    assertResult(Utils.timeFilter(testTimes(1), Some(Long.MaxValue))(legalLogs)) {
      Utils.timeFilter(testTimes(1), None)(legalLogs)
    }
    assertResult(Utils.timeFilter(testTimes(1), Some(Long.MaxValue))(unknownLogs ++ legalLogs ++ unknownLogs)) {
      Utils.timeFilter(testTimes(1), None)(unknownLogs ++ legalLogs ++ unknownLogs)
    }
    assertResult(Utils.timeFilter(Some(0L), testTimes(4))(legalLogs)) {
      Utils.timeFilter(None, testTimes(4))(legalLogs)
    }
    assertResult(Utils.timeFilter(Some(0L), testTimes(4))(unknownLogs ++ legalLogs ++ unknownLogs)) {
      Utils.timeFilter(None, testTimes(4))(unknownLogs ++ legalLogs ++ unknownLogs)
    }
  }
}

object UtilsSuite {

  val testDates: List[Date] =
    (0 to 4)
      .map(_.toString)
      .map("2017-03-21 12:00:0" + _)
      .map(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse)
      .toList

  val testTimes: List[Option[Long]] = testDates.map(_.getTime).map(time => Some(time))

  val legalLogs: List[LegalLog] =
    testDates.zipWithIndex.map { case (date, idx) =>
      new LegalLog("", date, isKeyLog = true, LvDebug, "", "", Map.empty) {
        override def toString: String = s"legal$idx"
      }
    }

  val equivocalLogsWithTime: List[EquivocalLog] =
    testDates.zipWithIndex.map { case (date, idx) =>
      new EquivocalLog("", Some(date), None, None, None, None, Map.empty) {
        override def toString: String = s"equivocal_time$idx"
      }
    }

  val equivocalLogsWithoutTime: List[EquivocalLog] =
    (0 to 4).map { i =>
      new EquivocalLog("", None, None, None, None, None, Map.empty) {
        override def toString: String = s"equivocal_no_time$i"
      }
    }.toList

  val unknownLogs: List[UnknownLog] =
    (0 to 4).map(i => new UnknownLog("") {
      override def toString: String = s"unknown$i"
    }).toList
}
