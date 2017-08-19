package com.example.zhiruili.loganalyzer.analyzer

import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzer.{AnalyzeResult, NoSuchProblemException}
import com.example.zhiruili.loganalyzer.analyzer.config.AnalyzerConfig.{HelpInfo, HelpInfoBinding}
import com.example.zhiruili.loganalyzer.logs.{LogItem, LogParser}
import com.example.zhiruili.loganalyzer.rules._

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * 日志分析器
  */
trait LogAnalyzer {

  /**
    * 按名读取规则
    *
    * @param ruleName 规则名
    * @return 对应规则，可能出错
    */
  def loadRuleByName(ruleName: String): Try[Rule]

  /**
    * 读取对应问题的帮助匹配规则
    *
    * @param problemCode  问题代号
    * @return 对应的匹配规则，可能出错
    */
  def loadHelpInfoBindings(problemCode: Int): Option[List[HelpInfoBinding]]

  def analyzeLogs(problemCode: Int)(logs: List[LogItem]): Try[AnalyzeResult] = {
    val tryHelpBinds = loadHelpInfoBindings(problemCode)
      .map(Success(_))
      .getOrElse(Failure(NoSuchProblemException(problemCode)))
    for {
      helpBinds <- tryHelpBinds
      res <- matchLogAndHelpBindings(logs, helpBinds)
    } yield res
  }

  def matchLogAndHelpBindings(logs: List[LogItem], helpBindings: List[HelpInfoBinding]): Try[AnalyzeResult] = {

    def matchHelper(helpBindings: List[HelpInfoBinding], res: AnalyzeResult): Try[AnalyzeResult] = {
      helpBindings match {
        case HelpInfoBinding(ruleName, helpInfo)::remainBinds =>
          loadRuleByName(ruleName) match {
            case Failure(thw) => Failure(thw)
            case Success(rule) => rule.matchLogItems(logs) match {
              case MatchSuccess(matched, _, _) => matchHelper(remainBinds, (matched, helpInfo)::res)
              case _ => matchHelper(remainBinds, res)
            }
          }
        case Nil => Success(res.reverse)
      }
    }
    matchHelper(helpBindings, Nil)
  }
}

object LogAnalyzer {

  case class NoSuchProblemException(problemCode: Int)
    extends RuntimeException(s"Can't find problem whose code is: $problemCode")

  type AnalyzeResult = List[(List[LogItem], HelpInfo)]

  /**
    * 根据问题代号分析日志字符串
    *
    * @param logString    日志字符串
    * @param problemCode  问题代号
    * @return 分析结果，可能出错
    */
  def analyzeLogString(logParser: LogParser)
                      (analyzer: LogAnalyzer)
                      (problemCode: Int)
                      (logString: String): Try[AnalyzeResult] = {
    for {
      logItems <- logParser.parseLogString(logString)
      res <- analyzer.analyzeLogs(problemCode)(logItems)
    } yield res
  }

  /**
    * 根据问题代号分析日志文件
    *
    * @param logParser    日志解析器
    * @param analyzer     日志分析器
    * @param problemCode  问题代号
    * @param filePath     日志文件路径
    * @return
    */
  def analyzeLogFile(logParser: LogParser)
                    (analyzer: LogAnalyzer)
                    (problemCode: Int)
                    (filePath: String): Try[AnalyzeResult] = {
    for {
      logString <- Try { Source.fromFile(filePath).mkString }
      res <- analyzeLogString(logParser)(analyzer)(problemCode)(logString)
    } yield res
  }
}

