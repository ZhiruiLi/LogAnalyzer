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

  type ErrBindings = Map[(String, Int), String]
  type GenBindings = Map[String, String]

  lazy val commentBindings: Try[CommentBindings] =
    CommentLoader.ofFile(configBaseDir, errorCommentFileName, generalCommentFileName).loadCommentBindings(currentSdk)
  lazy val errBindings: ErrBindings = commentBindings.map(_.errorBindings).get.toMap
  lazy val genBindings: GenBindings = commentBindings.map(_.generalBindings).get.toMap

  def loadProblemList: List[Problem] = configLoader.loadProblemList(currentSdk).get

  def analyzeLog(platform: Platform)(logItems: List[LogItem], problemTag: ProblemTag): Try[List[AnalyzeResult]] = {

    val loader = LogAnalyzerLoader(configLoader, ruleLoader)

    for {
      analyzer <- loader.loadAnalyzer(currentSdk, platform, "1.0.0")
      res <- analyzer.analyzeLogs(problemTag)(logItems)
    } yield res
  }

  def commentLog(logItem: LogItem): Option[String] = logItem match {
    case LegalLog(_, _, lv, _, msg, ext) =>
      val errComment = for {
        _ <- if (lv == LvError) Some(Unit) else None
        errCodeStr <- ext.get(LogItem.errCodeTag)
        errCode <- Try(errCodeStr.toInt).toOption
        errModule <- ext.get(LogItem.errModuleTag)
        comment <- errBindings.get((errModule, errCode))
      } yield comment
      val genComment = genBindings.get(msg)
      (errComment, genComment) match {
        case (None, None) => None
        case (Some(com), None) => Some(com)
        case (None, Some(com)) => Some(com)
        case (Some(com1), Some(com2)) => Some(com1 + "\n" + com2)
      }
    case UnknownLog(_) => None
  }
}
