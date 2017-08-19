package ui

import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzer.AnalyzeResult
import com.example.zhiruili.loganalyzer.analyzer.config.{ConfigLoader, DefaultConfigParser, FileConfigLoader}
import com.example.zhiruili.loganalyzer.analyzer.{LogAnalyzer, LogAnalyzerLoader}
import com.example.zhiruili.loganalyzer.logs.LogParser
import com.example.zhiruili.loganalyzer.rules.{BasicRuleParser, FileRuleLoader, RuleLoader}
import com.example.zhiruili.loganalyzer.{ILiveSdk, PlatformAndroid}

import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object ConsoleMain extends App {

  val baseDirPath = "./config/"
  val configLoader: ConfigLoader = FileConfigLoader.createSimpleLoader(baseDirPath, "_init_.json", DefaultConfigParser)
  val ruleLoader: RuleLoader = FileRuleLoader.createSimpleLoader(baseDirPath, BasicRuleParser)

  val analyzerLoader: LogAnalyzerLoader = LogAnalyzerLoader(configLoader, ruleLoader)

  val tryLogFileAnalyzer: Try[Int => String => Try[AnalyzeResult]] = for {
    analyzer <- analyzerLoader.loadAnalyzer(ILiveSdk, PlatformAndroid, "1.0.0")
  } yield LogAnalyzer.analyzeLogFile(LogParser)(analyzer)

  var ok = true
  while (ok) {
    promptPath()
    val inputPath = readLine()
    promptProblem()
    val inputProblem = readLine()
    ok = inputPath != null && inputProblem != null
    if (ok) {
      val analyzeRes = for {
        problemCode <- Try { inputProblem.toInt }
        analyzer <- tryLogFileAnalyzer
        res <- analyzer(problemCode)(inputPath)
      } yield res
      analyzeRes match {
        case Failure(thw) =>
          println(s"出现错误：$thw")
          println()
        case Success(Nil) =>
          println("未查询到帮助信息，请联系客服解决")
          println()
        case Success(res) =>
          println("查询到帮助信息：")
          res.foreach { case (_, helpInfo) =>
            println(s"帮助信息：${helpInfo.message}")
            helpInfo.helpPage.foreach(page => println(s"更多信息请参考：$page"))
            println()
          }
      }
    }
  }

  def promptPath(): Unit = println("请输入日志文件路径：")
  def promptProblem(): Unit = println("请输入问题代号：")
}
