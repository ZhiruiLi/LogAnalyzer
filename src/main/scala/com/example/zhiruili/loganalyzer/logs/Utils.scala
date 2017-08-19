package com.example.zhiruili.loganalyzer.logs

object Utils {

  /**
    * 过滤时间区间内的日志
    *
    * @param timeMin    最早时间(包含)
    * @param timeMax    最晚时间(包含)
    * @param logs       日志项
    * @return 日志项
    */
  def timeFilter(timeMin: Long, timeMax: Long)(logs: List[LogItem]): List[LogItem] = {

    def filterStart(originLogs: List[LogItem], keepLogsRev: List[LogItem]): List[LogItem] = originLogs match {
      case Nil => Nil
      case (LegalLog(date, _, _, _, _, _))::remain =>
        if (date.getTime < timeMin) filterStart(remain, Nil)
        else keepLogsRev.reverse ++ filterLogsFromHead(originLogs)
      case log::remain =>
        filterStart(remain, log::keepLogsRev)
    }

    def filterLogsFromHead(originLogs: List[LogItem]): List[LogItem] = {
      originLogs.takeWhile {
        case LegalLog(date, _, _, _, _, _) => date.getTime <= timeMax
        case _ => true
      }
    }

    filterStart(logs, Nil)
  }

}
