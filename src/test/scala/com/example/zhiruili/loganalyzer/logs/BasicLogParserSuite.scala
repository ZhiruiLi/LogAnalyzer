package com.example.zhiruili.loganalyzer.logs

import java.text.SimpleDateFormat
import java.util.Date

import com.example.zhiruili.loganalyzer.logs.BasicLogParser.BasicLogParsers
import com.example.zhiruili.loganalyzer.logs.TestLogs._
import org.scalatest.FunSuite

class BasicLogParsersSuite extends FunSuite {

  test("isKeyLog should only match KEY and DEV") {
    assert(BasicLogParsers.parseAll(BasicLogParsers.isKeyLog, "KEY").get)
    assert(!BasicLogParsers.parseAll(BasicLogParsers.isKeyLog, "DEV").get)
    assertThrows[RuntimeException](BasicLogParsers.parseAll(BasicLogParsers.isKeyLog, "KKK").get)
  }

  test("extMessage should match kv-pairs separated by | where each pair of key value should be divided by :") {
    def parse(str: String) = BasicLogParsers.parseAll(BasicLogParsers.extMessage, str).get
    assertResult(Map("abc" -> "def"))(parse("abc:def"))
    assertResult(Map("abc" -> "def"))(parse(" abc : def "))
    assertResult(Map("abc" -> "def", "123" -> "456"))(parse("abc:def|123:456"))
    assertResult(Map("abc" -> "def", "123" -> "456"))(parse(" abc : def | 123 : 456 "))
  }

  test("extMessage should allow item that only contains key") {
    def parse(str: String) = BasicLogParsers.parseAll(BasicLogParsers.extMessage, str).get
    assertResult(Map("abc" -> ""))(parse("abc:"))
    assertResult(Map("abc" -> ""))(parse(" abc : "))
    assertResult(Map("abc" -> ""))(parse("abc"))
    assertResult(Map("abc" -> ""))(parse(" abc "))
    assertResult(Map("abc" -> "", "def" -> ""))(parse("abc:|def"))
    assertResult(Map("abc" -> "", "def" -> ""))(parse(" abc : | def "))
  }

  test("Values of kv-pairs in extMessage can contains : , but can't contain |") {
    def parse(str: String) = BasicLogParsers.parseAll(BasicLogParsers.extMessage, str).get
    assertResult(Map("abc" -> "d:e:f"))(parse("abc:d:e:f"))
    assertResult(Map("abc" -> "d:e:f"))(parse(" abc : d:e:f "))
    assertResult(Map("abc" -> "d:e:f", "123" -> "4:5:6"))(parse("abc:d:e:f|123:4:5:6"))
    assertResult(Map("abc" -> "d:e:f", "123" -> "4:5:6"))(parse(" abc : d:e:f | 123 : 4:5:6 "))
  }

  test("extMessage can't be empty nor contains no keys") {
    def parse(str: String) = BasicLogParsers.parseAll(BasicLogParsers.extMessage, str).get
    assertThrows[RuntimeException](parse("   "))
    assertThrows[RuntimeException](parse("|"))
    assertThrows[RuntimeException](parse(":"))
    assertThrows[RuntimeException](parse(":abc"))
  }

  test("Legal logs should be parse to instances of LegalLog") {
    def parse(str: String) = BasicLogParser.parseLine(str).get
    assertResult(legalLog11)(parse(legalLogStr11))
    assertResult(legalLog12)(parse(legalLogStr12))
    assertResult(legalLog21)(parse(legalLogStr21))
    assertResult(legalLog22)(parse(legalLogStr22))
    assertResult(legalLog31)(parse(legalLogStr31))
    assertResult(legalLog32)(parse(legalLogStr32))
  }

  test("Illegal logs should be parse to instances of Unknown") {
    def parse(str: String) = BasicLogParser.parseLine(str).get
    assertResult(unknownLog1)(parse(illegalLogStr1))
    assertResult(unknownLog2)(parse(illegalLogStr2))
    assertResult(unknownLog3)(parse(illegalLogStr3))
    assertResult(unknownLog4)(parse(illegalLogStr4))
    assertResult(unknownLog5)(parse(illegalLogStr5))
    assertResult(unknownLog6)(parse(illegalLogStr6))
  }

  test("Multi enter chars between logs should be ignored") {
    val logStrs = List(
      legalLogStr11, illegalLogStr1, legalLogStr12, illegalLogStr2,
      legalLogStr21, illegalLogStr3, legalLogStr22, illegalLogStr4,
      legalLogStr31, illegalLogStr5, legalLogStr32, illegalLogStr6)
    val logStr1 = logStrs.mkString("\n")
    val logStr2 = logStrs.mkString("\r\n")
    val logStr3 = logStrs.mkString("\n\n\n")
    val logStr4 = logStrs.mkString("\r\n\r\n")
    val expectLogs = List(
      legalLog11, unknownLog1, legalLog12, unknownLog2,
      legalLog21, unknownLog3, legalLog22, unknownLog4,
      legalLog31, unknownLog5, legalLog32, unknownLog6)
    def parse(str: String) = BasicLogParser.parseString(str).get
    assertResult(expectLogs)(parse(logStr1))
    assertResult(expectLogs)(parse(logStr2))
    assertResult(expectLogs)(parse(logStr3))
    assertResult(expectLogs)(parse(logStr4))
  }
}

object TestLogs {
  val legalLogStr11 = "[2017-07-31 14:01:15][KEY][D][ILiveSDK][initSdk->init][aaa:123|bbb|ccc:]"
  val legalLogStr12 = "  [ 2017-07-31 14:01:15 ]  [KEY] [ D ][ILiveSDK]   [initSdk->init]  [ aaa :  123 | bbb | ccc : ] "
  val legalLogStr21 = "[2017-07-31 14:01:15][DEV][I][ILVBRoom][init entered]"
  val legalLogStr22 = "  [ 2017-07-31 14:01:15 ]  [DEV]  [I][  ILVBRoom][init entered  ]  "
  val legalLogStr31 = "[2017-07-31 14:01:15][DEV][E][IMCore][sso recv error][errCode:6012|module:sso_task]"
  val legalLogStr32 = " [ 2017-07-31 14:01:15 ][DEV  ][  E ]  [ IMCore][  sso recv error  ]  [ errCode:6012  |  module:sso_task] "
  val date: Date = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse("2017-07-31 14:01:15")
  val legalLog11 = new LegalLog(legalLogStr11, date, isKeyLog = true, LvDebug, "ILiveSDK", "initSdk->init", Map("aaa" -> "123", "bbb" -> "", "ccc" -> "")) {
    override def toString: String = "legal11"
  }
  val legalLog12 = new LegalLog(legalLogStr12, date, isKeyLog = true, LvDebug, "ILiveSDK", "initSdk->init", Map("aaa" -> "123", "bbb" -> "", "ccc" -> "")) {
    override def toString: String = "legal12"
  }
  val legalLog21 = new LegalLog(legalLogStr21, date, isKeyLog = false, LvInfo, "ILVBRoom", "init entered", Map.empty) {
    override def toString: String = "legal21"
  }
  val legalLog22 = new LegalLog(legalLogStr22, date, isKeyLog = false, LvInfo, "ILVBRoom", "init entered", Map.empty) {
    override def toString: String = "legal22"
  }
  val legalLog31 = new LegalLog(legalLogStr31, date, isKeyLog = false, LvError, "IMCore", "sso recv error", Map("errCode" -> "6012", "module" -> "sso_task")) {
    override def toString: String = "legal31"
  }
  val legalLog32 = new LegalLog(legalLogStr32, date, isKeyLog = false, LvError, "IMCore", "sso recv error", Map("errCode" -> "6012", "module" -> "sso_task")) {
    override def toString: String = "legal32"
  }
  // 非法的日期格式
  val illegalLogStr1 = "[07-31 14:01:15][KEY][D][ILiveSDK][initSdk->init][appId:1400|accountType:118]"
  // 非法的关键路径标签
  val illegalLogStr2 = "[2017-07-31 14:01:15][ABC][I] [ILVBRoom][init entered]"
  // 非法的日志等级
  val illegalLogStr3 = "[2017-07-31 14:01:15]   [DEV][X][ILVBRoom][init entered]"
  // 缺少结尾括号
  val illegalLogStr4 = "[2017-07-31 14:01:15][DEV][I][ILVBRoom][init"
  // 增加括号
  val illegalLogStr5 = "[2017-07-31 14:01:15][DEV][I][ILVB]Room][init entered]"
  // 其他字符串
  val illegalLogStr6 = "abcde 12345   hello[] "
  val unknownLog1 = new UnknownLog(illegalLogStr1) {
    override def toString: String = "unknown1"
  }
  val unknownLog2 = new UnknownLog(illegalLogStr2) {
    override def toString: String = "unknown2"
  }
  val unknownLog3 = new UnknownLog(illegalLogStr3) {
    override def toString: String = "unknown3"
  }
  val unknownLog4 = new UnknownLog(illegalLogStr4) {
    override def toString: String = "unknown4"
  }
  val unknownLog5 = new UnknownLog(illegalLogStr5) {
    override def toString: String = "unknown5"
  }
  val unknownLog6 = new UnknownLog(illegalLogStr6) {
    override def toString: String = "unknown6"
  }
}

