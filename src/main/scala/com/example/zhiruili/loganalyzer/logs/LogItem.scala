package com.example.zhiruili.loganalyzer.logs

import java.util.Date

/**
  * 日志等级
  */
sealed trait LogLevel {
  protected def levelVal: Int
  def >(that: LogLevel): Boolean = levelVal > that.levelVal
  def <(that: LogLevel): Boolean = levelVal < that.levelVal
}
case object LvDebug extends LogLevel { protected val levelVal = 1 }
case object LvInfo extends LogLevel { protected val levelVal = 2 }
case object LvWarn extends LogLevel { protected val levelVal = 3 }
case object LvError extends LogLevel { protected val levelVal = 4 }

/**
  * 日志项
  */
trait LogItem

/**
  * 合法日志项
  *
  * @param timestamp  时间戳，格式为 yyyy-MM-dd HH:mm:ss 或 yy-MM-dd HH:mm:ss
  * @param isKeyLog   是否为关键路径日志，取值为 KEY（表示是）或 DEV（表示否）
  * @param level      日志等级，取值为 D 或 I 或 W 或 E（严重等级依次升高）
  * @param position   打印日志的位置，例如 函数名、类名、文件名 等
  * @param message    日志基本信息字符串
  * @param extMessage 日志额外信息，键值对，用 : 分割键与值，用 | 分割多对键值，允许仅有键没有值
  */
case class LegalLog(timestamp: Date,
                    isKeyLog: Boolean,
                    level: LogLevel,
                    position: String,
                    message: String,
                    extMessage: Map[String, String]
                   ) extends LogItem

/**
  * 未知日志项
  *
  * @param originalLog  原始日志
  */
case class UnknownLog(originalLog: String) extends LogItem

object LogItem {
  // extMessage 部分错误码 key
  val errCodeTag: String = "errCode"
  // extMessage 部分出错模块 key
  val errModuleTag: String = "module"
}
