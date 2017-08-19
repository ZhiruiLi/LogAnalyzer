package ui

import java.text.SimpleDateFormat
import java.util.Date

import com.example.zhiruili.loganalyzer.{ILiveSdk, PlatformAndroid, Sdk}
import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzer.AnalyzeResult
import com.example.zhiruili.loganalyzer.analyzer.LogAnalyzerLoader
import com.example.zhiruili.loganalyzer.analyzer.config.{ConfigLoader, DefaultConfigParser, FileConfigLoader}
import com.example.zhiruili.loganalyzer.comment.{CommentBindings, CommentLoader}
import com.example.zhiruili.loganalyzer.logs._
import com.example.zhiruili.loganalyzer.rules.{BasicRuleParser, FileRuleLoader, RuleLoader}

import scala.io.Source
import scala.util.{Failure, Success, Try}
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.beans.binding.{Bindings, BooleanBinding, ObjectBinding}
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.{Node, Scene}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.stage.FileChooser
import scalafx.event.ActionEvent
import scalafx.scene.control.ScrollPane.ScrollBarPolicy
import scalafx.scene.control._
import scalafx.scene.paint.Color
import scalafx.scene.text.Text
import scalafx.scene.paint.Color._



object Renderer {

  object Formatter {

    val dateFormatter = new SimpleDateFormat("HH:mm:ss")

    def formatDate(date: Date): String = dateFormatter.format(date)

    def formatIsKey(isKeyLog: Boolean): String = if (isKeyLog) "*" else ""

    def formatLevel(lv: LogLevel): String = lv match {
      case LvDebug => "D"
      case LvInfo => "I"
      case LvWarn => "W"
      case LvError => "E"
    }

    def formatExt(ext: Map[String, String]): String = {
      ext.toList.map { case (k, v) => k + " -> " + v }.mkString(", ")
    }

    def formatLegalLog(log: LegalLog): String = {
      s"${formatIsKey(log.isKeyLog)}[${formatDate(log.timestamp)}] ${log.message} " +
        s"(${formatExt(log.extMessage)}) [${log.position}][${formatLevel(log.level)}]"
    }

    def formatUnknownLog(log: UnknownLog): String = {
      log.originalLog
    }
  }

  val levelColorMap: Map[LogLevel, Color] =
    Map(LvError -> Red, LvWarn -> Orange, LvInfo -> Blue, LvDebug -> Black)

  val defaultColor: Color = DarkGray

  def levelColor(lv: LogLevel): Color = levelColorMap.getOrElse(lv, defaultColor)

  def coloredText(txt: String, color: Color) = new Text {
    text = txt
    fill = color
  }

  def renderRichLog(logItem: LogItem, optComment: Option[String]): Node = logItem match {
    case log@LegalLog(_, _, lv, _, _, _) =>
      val color = levelColor(lv)
      val logStr = Formatter.formatLegalLog(log)
      val text = optComment.map(comment => s"$logStr\n$comment").getOrElse(logStr)
      coloredText(text, color)
    case log@UnknownLog(_) =>
      coloredText(Formatter.formatUnknownLog(log), defaultColor)
  }

  //  def renderHelpInfo(result: AnalyzeResult,
  //                     errCommentsMap: Map[(String, Int), String],
  //                     genCommentsMap: Map[String, String]): List[Node] = {
  //
  //    val res = result.map { case (logs, helpInfo) =>
  //      (renderLogItems(logs, errCommentsMap, genCommentsMap), helpInfo.message, helpInfo.helpPage)
  //    }
  //    if (res.isEmpty) List((Nil, "未能分析出相关问题", Some("https://www.qcloud.com/document/product/268/7752")))
  //    else res
  //  }
  //
  //  def renderLogItems(logItems: List[LogItem], errCommentsMap: Map[(String, Int), String], genCommentsMap: Map[String, String]): List[Node] = {
  //    logItems.flatMap(log => renderRichLog(log, errCommentsMap, genCommentsMap))
  //  }

}

object Analyzer {

  val currentSdk: Sdk = ILiveSdk
  val configFileName = "_init_.json"
  val configBaseDir = "./config/"
  val generalCommentFileName = "_comments_.json"
  val errorCommentFileName = "_error_comments_.json"
  val configLoader: ConfigLoader = FileConfigLoader.createSimpleLoader(configBaseDir, configFileName, DefaultConfigParser)
  val ruleLoader: RuleLoader = FileRuleLoader.createSimpleLoader(configBaseDir, BasicRuleParser)
  val analyzerLoader: LogAnalyzerLoader = LogAnalyzerLoader(configLoader, ruleLoader)
  val logParser: LogParser = LogParser

  type ErrBindings = Map[(String, Int), String]
  type GenBindings = Map[String, String]

  lazy val commentBindings: Try[CommentBindings] = CommentLoader.ofFile(configBaseDir, errorCommentFileName, generalCommentFileName).loadCommentBindings(currentSdk)
  lazy val errBindings: ErrBindings = commentBindings.map(_.errorBindings).get.toMap
  lazy val genBindings: GenBindings = commentBindings.map(_.generalBindings).get.toMap


  def analyzeLog(logItems: List[LogItem], problemCode: Int): Try[AnalyzeResult] = {

    val loader = LogAnalyzerLoader(
      FileConfigLoader.createSimpleLoader(configBaseDir, configFileName, DefaultConfigParser),
      FileRuleLoader.createSimpleLoader(configBaseDir, BasicRuleParser))

    for {
      analyzer <- loader.loadAnalyzer(currentSdk, PlatformAndroid, "1.0.0")
      res <- analyzer.analyzeLogs(problemCode)(logItems)
    } yield res
  }

  def commentLog(logItem: LogItem, errCommentsMap: ErrBindings, genCommentsMap: GenBindings): Option[String] = logItem match {
    case LegalLog(_, _, lv, _, msg, ext) =>
      val errComment = for {
        _ <- if (lv == LvError) Some(Unit) else None
        errCodeStr <- ext.get(LogItem.errCodeTag)
        errCode <- Try(errCodeStr.toInt).toOption
        errModule <- ext.get(LogItem.errModuleTag)
        comment <- errCommentsMap.get((errModule, errCode))
      } yield comment
      val genComment = genCommentsMap.get(msg)
      (errComment, genComment) match {
        case (None, None) => None
        case (Some(com), None) => Some(com)
        case (None, Some(com)) => Some(com)
        case (Some(com1), Some(com2)) => Some(com1 + "\n" + com2)
      }
    case UnknownLog(_) => None
  }
}

object AnalyzerApp extends JFXApp {

  val analyzeResult = new VBox
  val originalLogs = new VBox

  // 所有日志项
  val logList: ObjectProperty[List[LogItem]] = ObjectProperty(Nil)

  // 显示在 original log 区域的日志项
  val showNodes: ObjectProperty[List[Node]] = ObjectProperty(Nil)

  val defaultLabel = "请选择日志文件"
  val lbFileHint = Label(defaultLabel)
  val btnLoadFile = new Button {
    text = "导入本地日志文件"
    onAction = (e: ActionEvent) => {
      val chooser = new FileChooser
      val selectedFile = chooser.showOpenDialog(stage)
      if (selectedFile != null) {
        println(selectedFile)
        Try(Source.fromFile(selectedFile).mkString).flatMap(LogParser.parseLogString) match {
          case Success(items) =>
            logList() = items
            lbFileHint.text = selectedFile.getAbsolutePath
          case Failure(error) =>
            lbFileHint.text = error.getMessage
        }
      } else {
        lbFileHint.text() = defaultLabel
      }
    }
  }

  // 带有注释的日志项
  val allLogsWithComment: ObjectBinding[List[(LogItem, Option[String])]] = createObjectBinding(() => {
    logList().map(log => (log, Analyzer.commentLog(log, Analyzer.errBindings, Analyzer.genBindings)))
  }, logList)

  // 渲染后的日志项
  val renderedLogs: ObjectBinding[List[(LogItem, Node)]] = createObjectBinding(() => {
    allLogsWithComment().map { case (log, optComment) => (log, Renderer.renderRichLog(log, optComment)) }
  }, allLogsWithComment)

  val helpInfos: ObservableBuffer[(String, Option[String], List[LogItem])] = ObservableBuffer.empty
  val helpInfoNonNull: BooleanBinding = Bindings.createBooleanBinding(() => helpInfos.nonEmpty, helpInfos)

  renderedLogs.onChange {
    updateOriginList()
  }

  showNodes.onChange {
    originalLogs.children = showNodes()
  }

  val logListNotNull: BooleanBinding = createBooleanBinding(() => logList().nonEmpty, logList)

  val choiceProblem = new ChoiceBox(ObservableBuffer("有声音无画面", "无法接收消息", "音频事件无回调", "首帧事件无回调"))
  choiceProblem.selectionModel().selectFirst()
  val choicePlatform = new ChoiceBox(ObservableBuffer("Android", "iOS"))
  choicePlatform.selectionModel().selectFirst()

  val btnStartAnalyze = new Button("开始分析") {
    onAction = { _: ActionEvent =>
      println(choiceProblem.selectionModel().getSelectedIndex)
      println(choicePlatform.selectionModel().getSelectedIndex)
    }
  }

  val filterIsUpdated = BooleanProperty(false)

  val strNotRestraint: String = "无约束"

  val choiceIsKeyLog = new ChoiceBox(ObservableBuffer(strNotRestraint , "是", "否"))
  choiceIsKeyLog.selectionModel().selectFirst()
  choiceIsKeyLog.selectionModel().selectedIndexProperty().onChange {
    filterIsUpdated() = true
  }

  val choiceLogLevel = new ChoiceBox(ObservableBuffer(strNotRestraint, "Debug", "Info", "Warn", "Error"))
  choiceLogLevel.selectionModel().selectFirst()
  choiceLogLevel.selectionModel().selectedIndexProperty().onChange {
    filterIsUpdated() = true
  }

  val inputPosition = new TextField {
    onAction = { _: ActionEvent =>
      filterIsUpdated() = true
    }
  }

  val inputMessage = new TextField {
    onAction = { _: ActionEvent =>
      filterIsUpdated() = true
    }
  }

  val cbIsRegex = new CheckBox("正则匹配") {
    onAction = { _: ActionEvent =>
      filterIsUpdated() = true
    }
  }

  def updateOriginList(): Unit = {
    val filterKeyLog = choiceIsKeyLog.selectionModel().getSelectedIndex match {
      case 0 => None
      case 1 => Some(true)
      case 2 => Some(false)
    }
    val filterLogLevel = choiceLogLevel.selectionModel().getSelectedIndex match {
      case 0 => None
      case 1 => Some(LvDebug)
      case 2 => Some(LvInfo)
      case 3 => Some(LvWarn)
      case 4 => Some(LvError)
    }
    val isRegex = cbIsRegex.selected()
    def createRegex(str: String) = str.trim match {
      case "" => None
      case s =>
        Some(
          if (isRegex) s
          else s.flatMap { c =>
            if (c.isLetter)
              s"[${c.toUpper}${c.toLower}]"
            else if (c.isSpaceChar)
              s".*"
            else s"$c"
          }
        ).map(_.r)
    }
    val filterPosition = createRegex(inputPosition.text())
    val filterMessage = createRegex(inputMessage.text())
    val newNodes: List[Node] =
      renderedLogs()
        .filter {
          case (LegalLog(time, isKey, lv, pos, msg, _), _) =>
            val matchIsKey = filterKeyLog.forall(_ == isKey)
            val matchLv = filterLogLevel.forall(_ == lv)
            val matchPos = filterPosition.forall(_.findFirstIn(pos).nonEmpty)
            val matchMsg = filterMessage.forall(_.findFirstIn(msg).nonEmpty)
            matchIsKey && matchLv && matchPos && matchMsg
          case (UnknownLog(_), _) => true
        }
        .map(_._2)
    showNodes() = newNodes
  }

  val filterChecker = Timeline(KeyFrame(500 ms, "", { e: ActionEvent =>
    if (filterIsUpdated()) {
      filterIsUpdated() = false
      updateOriginList()
    }
  }))
  filterChecker.setCycleCount(Timeline.Indefinite)
  filterChecker.play()

  val logsFilterControllers = new HBox {
    spacing = 10
    alignment = Pos.CenterLeft
    visible <== logListNotNull
    children = Seq(
      Label("是否关键日志"), choiceIsKeyLog,
      Label("日志等级"), choiceLogLevel,
      Label("日志信息"), inputMessage,
      Label("打印位置"), inputPosition,
      cbIsRegex
    )
  }

  val mainContainer = new VBox {
    spacing = 10
    children = Seq(
      lbFileHint,
      new HBox {
        alignment = Pos.CenterLeft
        spacing = 10
        children = Seq(btnLoadFile, Label("问题描述"), choiceProblem, Label("运行平台"), choicePlatform, btnStartAnalyze)
      },
      new Label {
        text = "帮助信息"
        style = "-fx-font-size: 28pt"
        visible <== helpInfoNonNull
      },
      analyzeResult,
      new Label {
        text = "原始日志"
        style = "-fx-font-size: 28pt"
        visible <== logListNotNull
      },
      logsFilterControllers,
      originalLogs
    )
  }

  val mainScroll = new ScrollPane {
    padding = Insets(20)
    fitToWidth = true
    fitToHeight = true
    hbarPolicy = ScrollBarPolicy.Always
    vbarPolicy = ScrollBarPolicy.Always
    content = mainContainer
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "日志分析器"
    width = 1024
    height = 790
    scene = new Scene {
      content = mainScroll
    }
  }

  mainScroll.prefWidth <== stage.width
  mainScroll.prefHeight <== stage.height - 22
}
