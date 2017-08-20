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
    testDates.map(date => LegalLog(date, isKeyLog = true, LvDebug, "", "", Map.empty))

  val unknownLogs: List[UnknownLog] =
    (0 to 4).map(_.toString).map(UnknownLog).toList
}
