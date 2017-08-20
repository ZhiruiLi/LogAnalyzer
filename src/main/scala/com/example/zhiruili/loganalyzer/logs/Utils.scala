package com.example.zhiruili.loganalyzer.logs

object Utils {

  /**
    * 过滤时间区间内的日志
    *
    * @param timeStart  最早时间(包含)
    * @param timeEnd    最晚时间(包含)
    * @param logs       日志项
    * @return 日志项
    */
  def timeFilter(timeStart: Option[Long], timeEnd: Option[Long])(logs: List[LogItem]): List[LogItem] = {

    def filterStart(originLogs: List[LogItem], keepLogsRev: List[LogItem]): List[LogItem] = (timeStart, originLogs) match {
      case (_, Nil) =>
        Nil
      case (None, _) =>
        filterLogsFromHead(originLogs)
      case (Some(start), (log: LegalLog)::remain) =>
        if (log.timestamp.getTime < start) {
          filterStart(remain, Nil)
        } else {
          filterLogsFromHead(keepLogsRev.reverse ++ originLogs)
        }
      case (_, log::remain) =>
        filterStart(remain, log::keepLogsRev)
    }

    def filterLogsFromHead(originLogs: List[LogItem]): List[LogItem] = timeEnd match {
      case None =>
        originLogs
      case Some(end) =>
        originLogs.takeWhile {
          case log: LegalLog => log.timestamp.getTime <= end
          case _ => true
        }
    }

    filterStart(logs, Nil)
  }

}
