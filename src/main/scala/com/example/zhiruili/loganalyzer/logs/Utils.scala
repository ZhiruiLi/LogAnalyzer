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
      case (None, _) => originLogs
      case (_, Nil) => Nil
      case (Some(timeMin), LegalLog(date, _, _, _, _, _)::remain) =>
        if (date.getTime < timeMin) filterStart(remain, Nil)
        else {
          val tailLogs = timeEnd match {
            case None => originLogs
            case Some(timeMax) => filterLogsFromHead(originLogs, timeMax)
          }
          keepLogsRev.reverse ++ tailLogs
        }
      case (_, log::remain) =>
        filterStart(remain, log::keepLogsRev)
    }

    def filterLogsFromHead(originLogs: List[LogItem], timeMax: Long): List[LogItem] = {
      originLogs.takeWhile {
        case LegalLog(date, _, _, _, _, _) => date.getTime <= timeMax
        case _ => true
      }
    }

    filterStart(logs, Nil)
  }

}
