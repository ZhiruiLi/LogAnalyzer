package com.example.zhiruili.loganalyzer.analyzer

import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzer.AnalyzeResult
import AnalyzerConfig.{HelpInfo, HelpInfoBinding, ProblemTag}
import com.example.zhiruili.loganalyzer
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
    * @param problemTag  问题标签
    * @return 对应的匹配规则，可能出错
    */
  def loadHelpInfoBindings(problemTag: ProblemTag): Try[List[HelpInfoBinding]]

  /**
    * 根据问题标签分析日志
    *
    * @param problemTag   问题标签
    * @param logs         日志列表
    * @return 分析结果列表，可能出错
    */
  def analyzeLogs(problemTag: ProblemTag)(logs: List[LogItem]): Try[List[AnalyzeResult]] = {
    for {
      helpBinds <- loadHelpInfoBindings(problemTag)
      res <- matchLogAndHelpBindings(logs, helpBinds)
    } yield res
  }

  def matchLogAndHelpBindings(logs: List[LogItem], helpBindings: List[HelpInfoBinding]): Try[List[AnalyzeResult]] = {

    def matchHelper(helpBindings: List[HelpInfoBinding], res: List[AnalyzeResult]): Try[List[AnalyzeResult]] = {
      helpBindings match {
        case HelpInfoBinding(ruleName, helpInfo)::remainBinds =>
          loadRuleByName(ruleName) match {
            case Failure(thw) => Failure(thw)
            case Success(rule) => rule.matchLogItems(logs) match {
              case MatchSuccess(matched, _, _) => matchHelper(remainBinds, AnalyzeResult(matched, helpInfo)::res)
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

  case class AnalyzeResult(relatedLogs: List[LogItem], helpInfo: HelpInfo)

  /**
    * 根据问题代号分析日志字符串
    *
    * @param logString    日志字符串
    * @param problemTag   问题标签
    * @return 分析结果，可能出错
    */
  def analyzeLogString(logParser: LogParser)
                      (analyzer: LogAnalyzer)
                      (problemTag: ProblemTag)
                      (logString: String): Try[List[AnalyzeResult]] = {
    for {
      logItems <- logParser.parseString(logString)
      res <- analyzer.analyzeLogs(problemTag)(logItems)
    } yield res
  }

  /**
    * 根据问题代号分析日志文件
    *
    * @param logParser    日志解析器
    * @param analyzer     日志分析器
    * @param problemTag   问题标签
    * @param filePath     日志文件路径
    * @return
    */
  def analyzeLogFile(logParser: LogParser)
                    (analyzer: LogAnalyzer)
                    (problemTag: ProblemTag)
                    (filePath: String): Try[List[AnalyzeResult]] = {
    for {
      logString <- Try { Source.fromFile(filePath)(loganalyzer.encoding).mkString }
      res <- analyzeLogString(logParser)(analyzer)(problemTag)(logString)
    } yield res
  }
}

