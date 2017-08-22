package ui

import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.{Problem, ProblemTag}
import com.example.zhiruili.loganalyzer.{ILiveSdk, Platform, Sdk}
import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzer.AnalyzeResult
import com.example.zhiruili.loganalyzer.analyzer.{ConfigLoader, LogAnalyzerLoader}
import com.example.zhiruili.loganalyzer.analyzer.config.FileConfigLoader
import com.example.zhiruili.loganalyzer.comment.{CommentBindings, CommentLoader}
import com.example.zhiruili.loganalyzer.logs._
import com.example.zhiruili.loganalyzer.rules.{BasicRuleParser, FileRuleLoader, RuleLoader}

import scala.util.Try

object Analyzer {

  val currentSdk: Sdk = ILiveSdk
  val helpBindConfigName = "_init_.json"
  val problemConfigName = "_problems_.json"
  val configBaseDir = "./config/"
  val generalCommentFileName = "_comments_.json"
  val errorCommentFileName = "_error_comments_.json"
  val configLoader: ConfigLoader = FileConfigLoader.createSimpleLoader(configBaseDir, helpBindConfigName, problemConfigName)
  val ruleLoader: RuleLoader = FileRuleLoader.createSimpleLoader(configBaseDir, BasicRuleParser)
  val analyzerLoader: LogAnalyzerLoader = LogAnalyzerLoader(configLoader, ruleLoader)
  val logParser: LogParser = BasicLogParser

  type ErrBindings = Map[String, Map[Int, String]]
  type GenBindings = Map[String, String]

  lazy val commentBindings: Try[CommentBindings] =
    CommentLoader.ofFile(configBaseDir, errorCommentFileName, generalCommentFileName).loadCommentBindings(currentSdk)
  lazy val errBindings: ErrBindings =
    commentBindings.map(_.errorBindings).get.map { case (name, lst) => (name, lst.toMap) }.toMap
  lazy val genBindings: GenBindings = commentBindings.map(_.generalBindings).get.toMap

  def loadProblemList: List[Problem] = configLoader.loadProblemList(currentSdk).get

  def analyzeLog(platform: Platform)(logItems: List[LogItem], problemTag: ProblemTag): Try[List[AnalyzeResult]] = {

    val loader = LogAnalyzerLoader(configLoader, ruleLoader)

    for {
      analyzer <- loader.loadAnalyzer(currentSdk, platform, "1.0.0")
      res <- analyzer.analyzeLogs(problemTag)(logItems)
    } yield res
  }

  def commentLog(logItem: LogItem): List[String] = logItem match {
    case LegalLog(_, _, _, lv, _, msg, ext) =>
      val errComment = for {
        _ <- if (lv == LvError) Some(Unit) else None
        errCodeStr <- ext.get(LogItem.errCodeTag)
        errCode <- Try(errCodeStr.toInt).toOption
        errModule <- ext.get(LogItem.errModuleTag)
        comment <- errBindings.get(errModule).flatMap(_.get(errCode))
      } yield comment
      val genComment = genBindings.get(msg)
      (errComment, genComment) match {
        case (None, None) => Nil
        case (Some(com), None) => List(com)
        case (None, Some(com)) => List(com)
        case (Some(com1), Some(com2)) => List(com1, com2)
      }
    case EquivocalLog(_, _, _, optLv, _, optMsg, ext) =>
      val errComments: List[String] = for {
        lv <- optLv.toList
        if lv == LvError
        errCodeStr <- ext.get(LogItem.errCodeTag).toList
        errCode <- Try(errCodeStr.toInt).toOption.toList
        comment <- errBindings.flatMap { case (_, map) => map.get(errCode) }.toList
      } yield comment
      val genComment = optMsg.flatMap(msg => genBindings.get(msg)).toList
      errComments ++ genComment
    case UnknownLog(_) => Nil
  }
}
