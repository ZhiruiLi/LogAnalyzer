package com.example.zhiruili.loganalyzer.rules

import com.example.zhiruili.loganalyzer.logs.LogItem

sealed trait MatchResult
case class MatchSuccess(matchLogs: List[LogItem], skipLogs: List[LogItem], remainLogs: List[LogItem]) extends MatchResult
case class MatchFailure(mismatchLogs: List[LogItem], matchLogs: List[LogItem], skipLogs: List[LogItem], remainLogs: List[LogItem]) extends MatchResult
// case class MatchError(thw: Throwable) extends MatchResult

/**
  * 匹配规则
  */
trait Rule {

  /**
    * 匹配一组日志
    *
    * @param logs 日志列表
    * @return 如果匹配成功，返回 MatchSuccess，纪录匹配信息，如果匹配失败返回 MatchFailure
    */
  def matchLogItems(logs: List[LogItem]): MatchResult
}
