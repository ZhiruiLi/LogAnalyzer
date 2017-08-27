package com.example.zhiruili.loganalyzer.ui

import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.{Problem, ProblemTag}
import com.example.zhiruili.loganalyzer._
import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzer.AnalyzeResult
import com.example.zhiruili.loganalyzer.analyzer.{ConfigLoader, LogAnalyzerLoader}
import com.example.zhiruili.loganalyzer.analyzer.config.FileConfigLoader
import com.example.zhiruili.loganalyzer.comment.{CommentBindings, CommentLoader}
import com.example.zhiruili.loganalyzer.logs._
import com.example.zhiruili.loganalyzer.rules.{BasicRuleParser, FileRuleLoader, RuleLoader}

import scala.util.Try

object AnalyzerHelper {

  val currentSdk: Sdk = ILiveSdk
  val helpBindConfigName = "_init_.json"
  val problemConfigName = "_problems_.json"
  val configBaseDir = "./config/"
  val generalCommentFileName = "_comments_.json"
  val errorCommentFileName = "_error_comments_.json"
  val configLoader: ConfigLoader = FileConfigLoader.createSimpleLoader(configBaseDir, helpBindConfigName, problemConfigName)
  val ruleLoader: RuleLoader = FileRuleLoader.createSimpleLoader(configBaseDir, BasicRuleParser)
  val analyzerLoader: LogAnalyzerLoader = LogAnalyzerLoader(configLoader, ruleLoader)

  val platformToLogParser: Map[Platform, LogParser] = Map(
    PlatformAndroid -> LogParserDelegateChain(BasicLogParser, List(ILiveLegacyLogParsers.ofAndroid)),
    PlatformIOS -> LogParserDelegateChain(BasicLogParser, List(ILiveLegacyLogParsers.ofIOS)))

  type ErrBindings = Map[String, Map[Int, String]]
  type GenBindings = Map[String, String]

  // 获取指定平台的注释绑定关系
  val commentBindings: Map[Platform, CommentBindings] = {
    val loader = CommentLoader.ofFile(configBaseDir, errorCommentFileName, generalCommentFileName)
    Map(
      PlatformAndroid -> loader.loadCommentBindings(currentSdk, PlatformAndroid).get,
      PlatformIOS -> loader.loadCommentBindings(currentSdk, PlatformIOS).get)
  }

  /**
    * 读取问题列表
    *
    * @return 问题列表
    */
  def loadProblemList: Try[List[Problem]] = configLoader.loadProblemList(currentSdk)

  /**
    * 分析日志
    *
    * @param platform     运行平台
    * @param logItems     日志项
    * @param problemTag   问题标签
    * @return 分析结果列表，可能出错
    */
  def analyzeLog(platform: Platform)(logItems: List[LogItem], problemTag: ProblemTag): Try[List[AnalyzeResult]] = {

    val loader = LogAnalyzerLoader(configLoader, ruleLoader)

    for {
      analyzer <- loader.loadAnalyzer(currentSdk, platform, "1.0.0")
      res <- analyzer.analyzeLogs(problemTag)(logItems)
    } yield res
  }

  def commentLog(logItem: LogItem, platform: Platform): List[String] = logItem match {
    case LegalLog(_, _, _, lv, _, msg, ext) =>
      val errComment = for {
        _ <- if (lv == LvError) Some(Unit) else None
        errCodeStr <- ext.get(LogItem.errCodeTag)
        errCode <- Try(errCodeStr.toInt).toOption
        errModule <- ext.get(LogItem.errModuleTag)
        comment <- commentBindings.get(platform).flatMap(_.matchError(errCode, errModule))
      } yield comment
      (errComment, commentBindings.get(platform).flatMap(_.matchDistinctMessage(msg))) match {
        case (None, None) => Nil
        case (Some(com), None) => List(com)
        case (None, Some(com)) => List(com)
        case (Some(com1), Some(com2)) => List(com1, com2)
      }
    case EquivocalLog(_, _, _, optLv, _, optMsg, ext) =>
      val errComments: List[String] = (for {
        lv <- optLv
        if lv == LvError
        errCodeStr <- ext.get(LogItem.errCodeTag)
        errCode <- Try(errCodeStr.toInt).toOption
        binding <- commentBindings.get(platform)
      } yield binding.matchErrorCode(errCode)).getOrElse(Nil)
      val genComment = (for {
        msg <- optMsg
        binding <- commentBindings.get(platform)
      } yield binding.matchFuzzyMessage(msg)).getOrElse(Nil)
      errComments ++ genComment
    case UnknownLog(_) => Nil
  }
}
