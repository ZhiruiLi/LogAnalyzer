package com.example.zhiruili.loganalyzer.logs

import java.text.SimpleDateFormat

import org.scalatest.FunSuite

import scala.util.Success

class ILiveLegacyLogParsersSuite extends FunSuite {

  object AndroidLogs {

    import ILiveLegacyLogParsers.ofAndroid._

    val formatter = new SimpleDateFormat("yy-MM-dd HH:mm:ss")

    val logStr1 = "17-08-16 16:37:45|E|1|ILVB-CallMgr|sendC2CMessage error6011: to user invalid|cmd:123"

    val log1 = EquivocalLog(
      logStr1,
      Some(formatter.parse("17-08-16 16:37:45")),
      Some(false),
      Some(LvError),
      Some("ILVB-CallMgr"),
      Some("sendC2CMessage error6011: to user invalid|cmd:123"),
      Map(threadIdTag -> "1", LogItem.errCodeTag -> "6011")
    )

    val logStr2 = "17-08-16 16:37:45|D|1|ILVB-CallMgr|Key_Procedure|ILVB-Call|invitePrivateMember->failed:10004|message"

    val log2 = EquivocalLog(
      logStr2,
      Some(formatter.parse("17-08-16 16:37:45")),
      Some(true),
      Some(LvError),
      Some("ILVB-CallMgr"),
      Some("ILVB-Call|invitePrivateMember->failed:10004|message"),
      Map(threadIdTag -> "1", LogItem.errCodeTag -> "10004")
    )

    val logStr3 = "17-08-16 16:39:03|I|123|ILVBRoom|Key_Procedure|sendC2CMessage->id:6003"

    val log3 = EquivocalLog(
      logStr3,
      Some(formatter.parse("17-08-16 16:39:03")),
      Some(true),
      Some(LvInfo),
      Some("ILVBRoom"),
      Some("sendC2CMessage->id:6003"),
      Map(threadIdTag -> "123")
    )
  }

  object IOSLogs {

    val formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

    val logStr1 = "[2017-07-24 10:48:57][INFO][ILiveSDK] | ILiveLogin:Key_Procedure|iLiveLogin|succ"

    val log1 = EquivocalLog(
      logStr1,
      Some(formatter.parse("2017-07-24 10:48:57")),
      Some(true),
      Some(LvInfo),
      Some("ILiveLogin"),
      Some("iLiveLogin|succ"),
      Map()
    )

    val logStr2 = "[2017-07-24 10:49:01][DEBUG][ILiveSDK] | ILiveRoom:Other_Procedure|switchCamera|start:pos:1"

    val log2 = EquivocalLog(
      logStr2,
      Some(formatter.parse("2017-07-24 10:49:01")),
      Some(false),
      Some(LvDebug),
      Some("ILiveRoom"),
      Some("switchCamera|start:pos:1"),
      Map()
    )

    val logStr3 = "[2017-07-24 10:49:02][INFO][ILiveSDK] | ILiveRoom:Key_Procedure|balabalaerror->123XXX"

    val log3 = EquivocalLog(
      logStr3,
      Some(formatter.parse("2017-07-24 10:49:02")),
      Some(true),
      Some(LvError),
      Some("ILiveRoom"),
      Some("balabalaerror->123XXX"),
      Map(LogItem.errCodeTag -> "123")
    )
  }

  test("Test some logs for Android") {
    assertResult(Success(AndroidLogs.log1)) {
      ILiveLegacyLogParsers.ofAndroid.parseLine(AndroidLogs.logStr1)
    }
    assertResult(Success(AndroidLogs.log2)) {
      ILiveLegacyLogParsers.ofAndroid.parseLine(AndroidLogs.logStr2)
    }
    assertResult(Success(AndroidLogs.log3)) {
      ILiveLegacyLogParsers.ofAndroid.parseLine(AndroidLogs.logStr3)
    }
  }

  test("Test some logs for iOS") {
    assertResult(Success(IOSLogs.log1)) {
      ILiveLegacyLogParsers.ofIOS.parseLine(IOSLogs.logStr1)
    }
    assertResult(Success(IOSLogs.log2)) {
      ILiveLegacyLogParsers.ofIOS.parseLine(IOSLogs.logStr2)
    }
    assertResult(Success(IOSLogs.log3)) {
      ILiveLegacyLogParsers.ofIOS.parseLine(IOSLogs.logStr3)
    }
  }
}
