package com.example.zhiruili.loganalyzer.logs

object Utils {

  /**
    * 过滤时间区间内的日志
    *
    * @param timeStart  最早时间(包含)
    * @param timeEnd    最晚时间(包含)
    * @param logs       日志项
    * @param asLogItem  隐式转换到 LogItem
    * @tparam T         任意可以被看作 LogItem 的对象
    * @return           筛选后的日志项
    */
  def timeFilter[T](timeStart: Option[Long], timeEnd: Option[Long])(logs: List[T])(implicit asLogItem: T => LogItem): List[T] = {

    def filterStart(originLogs: List[T], keepLogsRev: List[T]): List[T] = (timeStart, originLogs) match {
      case (_, Nil) =>
        Nil
      case (None, _) =>
        filterLogsFromHead(originLogs)
      case (Some(start), logLikeVal::remain) => asLogItem(logLikeVal) match {
        case log: LegalLog =>
          if (log.timestamp.getTime < start) {
            filterStart(remain, Nil)
          } else {
            filterLogsFromHead(keepLogsRev.reverse ++ originLogs)
          }
        case _ =>
          println(s"unknown log: $logLikeVal")
          filterStart(remain, logLikeVal::keepLogsRev)
      }
    }

    def filterLogsFromHead(originLogs: List[T]): List[T] = timeEnd match {
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
