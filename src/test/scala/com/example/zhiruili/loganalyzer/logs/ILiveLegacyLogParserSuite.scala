package com.example.zhiruili.loganalyzer.logs

import java.text.SimpleDateFormat

import org.scalatest.FunSuite

import scala.util.Success

class ILiveLegacyLogParserSuite extends FunSuite {
  object Logs {

    val formatter = new SimpleDateFormat("yy-MM-dd HH:mm:ss")

    val logStr1 = "17-08-16 16:37:45|E|1|ILVB-CallMgr|sendC2CMessage error6011: to user invalid|cmd:123"

    val log1 = EquivocalLog(
      logStr1,
      Some(formatter.parse("17-08-16 16:37:45")),
      Some(false),
      Some(LvError),
      Some("ILVB-CallMgr"),
      Some("sendC2CMessage error6011: to user invalid|cmd:123"),
      Map(ILiveLegacyLogParser.threadIdTag -> "1", LogItem.errCodeTag -> "6011")
    )

    val logStr2 = "17-08-16 16:37:45|D|1|ILVB-CallMgr|Key_Procedure|ILVB-Call|invitePrivateMember->failed:10004|message"

    val log2 = EquivocalLog(
      logStr2,
      Some(formatter.parse("17-08-16 16:37:45")),
      Some(true),
      Some(LvError),
      Some("ILVB-CallMgr"),
      Some("ILVB-Call|invitePrivateMember->failed:10004|message"),
      Map(ILiveLegacyLogParser.threadIdTag -> "1", LogItem.errCodeTag -> "10004")
    )

    val logStr3 = "17-08-16 16:39:03|I|123|ILVBRoom|Key_Procedure|sendC2CMessage->id:6003"

    val log3 = EquivocalLog(
      logStr3,
      Some(formatter.parse("17-08-16 16:39:03")),
      Some(true),
      Some(LvInfo),
      Some("ILVBRoom"),
      Some("sendC2CMessage->id:6003"),
      Map(ILiveLegacyLogParser.threadIdTag -> "123")
    )
  }
  test("Test some logs") {
    assertResult(Success(Logs.log1)) {
      ILiveLegacyLogParser.parseLine(Logs.logStr1)
    }
    assertResult(Success(Logs.log2)) {
      ILiveLegacyLogParser.parseLine(Logs.logStr2)
    }
    assertResult(Success(Logs.log3)) {
      ILiveLegacyLogParser.parseLine(Logs.logStr3)
    }
  }
}
