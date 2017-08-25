package com.example.zhiruili.loganalyzer.ui

import com.example.zhiruili.loganalyzer._
import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.{HelpInfo, Problem, ProblemTag}
import com.example.zhiruili.loganalyzer.logs._
import com.example.zhiruili.utils.Utils._

import scala.util.{Failure, Success}
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.beans.binding.{Bindings, BooleanBinding, ObjectBinding}
import scalafx.beans.property.{BooleanProperty, ObjectProperty, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.{Node, Scene}
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.stage.FileChooser
import scalafx.event.ActionEvent
import scalafx.scene.control._
import scalafx.scene.input.KeyEvent
import scalafx.scene.text.{Text, TextFlow}

object AnalyzerApp extends JFXApp {

  object Implicits {
    implicit val logItemPairAsLogItem: ((LogItem, Node)) => LogItem = _._1
  }

  import Implicits._

  // --------------- 公用 ------------------------

  val platformChoiceBox = new ChoiceBox(ObservableBuffer("Android", "iOS"))
  platformChoiceBox.selectionModel().selectFirst()
  val platforms = Vector(PlatformAndroid, PlatformIOS)

  def selectedPlatform: Platform = platforms(platformChoiceBox.selectionModel().getSelectedIndex)

  // --------------- 原始日志信息 ------------------

  val originalLogsContainer = new VBox

  // 所有日志项
  val logList: ObjectProperty[List[LogItem]] = ObjectProperty(Nil)

  // 错误信息
  val errMessage: StringProperty = StringProperty("")
  val errMessageText = new TextFlow(
    new Text { text <== errMessage }
  )
  def setError(str: String): Unit = {
    errMessage() = str
  }
  def clearError(): Unit = {
    errMessage() = ""
  }

  // 显示在 original log 区域的节点
  val originalLogShowNodes: ObjectProperty[List[Node]] = ObjectProperty(Nil)

  val defaultLabelText = "请选择日志文件"
  val fileHintLabel = Label(defaultLabelText)
  val loadFileButton = new Button {
    text = "导入本地日志文件"
    onAction = (_: ActionEvent) => {
      clearError()
      val chooser = new FileChooser
      val selectedFile = chooser.showOpenDialog(stage)
      if (selectedFile != null) {
        println(selectedFile)
        Analyzer.platformToLogParser(selectedPlatform).parseFile(selectedFile) match {
          case Success(items) =>
            logList() = items
            fileHintLabel.text = selectedFile.getAbsolutePath
          case Failure(error) =>
            fileHintLabel.text = error.getMessage
        }
      } else {
        fileHintLabel.text() = defaultLabelText
      }
    }
  }

  // 带有注释的日志项
  val allLogsWithComment: ObjectBinding[List[(LogItem, List[String])]] = createObjectBinding(() => {
    logList().map(log => (log, Analyzer.commentLog(log, selectedPlatform)))
  }, logList)

  // 渲染后的日志项
  val renderedLogs: ObjectBinding[List[(LogItem, Node)]] = createObjectBinding(() => {
    allLogsWithComment().map { case (log, comments) => (log, Renderer.renderRichLog(log, comments)) }
  }, allLogsWithComment)

  renderedLogs.onChange {
    clearError()
    updateOriginLogList()
  }

  originalLogShowNodes.onChange {
    clearError()
    originalLogsContainer.children = originalLogShowNodes()
  }

  val filterIsUpdated = BooleanProperty(false)
  def setFilterExpired(): Unit = {
    filterIsUpdated() = true
  }

  // 过滤关键日志
  val filterIsKeyLogChoiceBox = new ChoiceBox(ObservableBuffer("无约束", "是", "否"))
  filterIsKeyLogChoiceBox.selectionModel().selectFirst()
  filterIsKeyLogChoiceBox.selectionModel().selectedIndexProperty().onChange {
    setFilterExpired()
  }
  def filterOptIsKeyLog: Option[Boolean] = filterIsKeyLogChoiceBox.selectionModel().getSelectedIndex match {
    case 0 => None
    case 1 => Some(true)
    case 2 => Some(false)
  }

  // 过滤最低等级日志
  val minLogLevelChoiceBox = new ChoiceBox(ObservableBuffer("Verbose", "Debug", "Info", "Warn", "Error"))
  minLogLevelChoiceBox.selectionModel().selectFirst()
  minLogLevelChoiceBox.selectionModel().selectedIndexProperty().onChange {
    setFilterExpired()
  }
  def filterMinLogLevel: LogLevel = minLogLevelChoiceBox.selectionModel().getSelectedIndex match {
    case 0 => LvVerbose
    case 1 => LvDebug
    case 2 => LvInfo
    case 3 => LvWarn
    case 4 => LvError
  }

  // 日志信息过滤输入框
  val filterMessageTextField: TextField = new TextField { onKeyTyped = { _: KeyEvent => setFilterExpired() } }
  def filterMessage: Option[String] = filterMessageTextField.text() match {
    case "" => None
    case str => Some(if (isStrictMode) str else createFuzzyRegex(str))
  }

  // 日志打印位置过滤输入框
  val filterPositionTextField: TextField = new TextField { onKeyTyped = { _: KeyEvent => setFilterExpired() } }
  def filterPosition: Option[String] = filterPositionTextField.text() match {
    case "" => None
    case str => Some(if (isStrictMode) str else createFuzzyRegex(str))
  }

  // 是否采用正则匹配，未选择则为模糊匹配
  val isStrictModeCheckBox = new CheckBox("严格匹配(正则)") { onAction = { _: ActionEvent => setFilterExpired() } }
  def isStrictMode: Boolean = isStrictModeCheckBox.selected()

  // 输出结果是否包含无法解析的日志
  val includeUnknownCheckBox = new CheckBox("结果包含无法解析的日志") { onAction = { _: ActionEvent => setFilterExpired() } }
  def includeUnknown: Boolean = includeUnknownCheckBox.selected()

  // 过滤时间区间
  val originLogStartTimeTextField: DateTimeTextField = DateTimeTextField(doOnInputLegal = Some(() => setFilterExpired()))
  val originLogEndTimeTextField: DateTimeTextField = DateTimeTextField(doOnInputLegal = Some(() => setFilterExpired()))

  // 更新日志浏览区域的日志内容
  def updateOriginLogList(): Unit = {
    val logsAfterFilter =
      logs.Utils.timeFilter(originLogStartTimeTextField.getTime, originLogEndTimeTextField.getTime)(renderedLogs())
    val matchRegex = (str: String, regexStr: String) => regexStr.r.findFirstIn(str).nonEmpty
    val newNodes: List[Node] = logsAfterFilter
      .filter {
        case (LegalLog(_, _, isKey, lv, pos, msg, _), _) =>
          def matchIsKey = filterOptIsKeyLog.forall(_ == isKey)
          def matchLv = filterMinLogLevel <= lv
          def matchPos = filterPosition.forall(_.r.findFirstIn(pos).nonEmpty)
          def matchMsg = filterMessage.forall(_.r.findFirstIn(msg).nonEmpty)
          matchIsKey && matchLv && matchPos && matchMsg
        case (EquivocalLog(_, _, optIsKey, optLv, optPos, optMsg, _), _) =>
          def matchIsKey = testIfDefined(optIsKey, filterOptIsKeyLog, (b: Boolean, fB: Boolean) => b == fB)
          def matchLv = optLv.forall(_ >= filterMinLogLevel)
          def matchPos = testIfDefined(optPos, filterPosition, matchRegex)
          def matchMsg = testIfDefined(optMsg, filterMessage, matchRegex)
          matchIsKey && matchLv && matchPos && matchMsg
        case (UnknownLog(log), _) =>
          includeUnknown &&
            testIfDefined(Some(log), filterPosition, matchRegex) &&
            testIfDefined(Some(log), filterMessage, matchRegex)
      }
      .map(_._2)
    originalLogShowNodes() = newNodes
  }

  // 监视日志过滤选项的刷新
  val refreshChecker: (ActionEvent) => Unit = { _ =>
    if (filterIsUpdated()) {
      filterIsUpdated() = false
      updateOriginLogList()
    }
  }
  val filterChecker = Timeline(KeyFrame(500.ms, "", refreshChecker))
  filterChecker.setCycleCount(Timeline.Indefinite)
  filterChecker.play()

  // --------------- 帮助信息 ---------------------

  val analyzeResultContainer = new VBox {
    spacing = 10
  }

  // 帮助信息
  val optHelpInfos: ObjectProperty[Option[List[(HelpInfo, List[LogItem])]]] = ObjectProperty(None)

  // 显示在 analyze result 区域的节点
  val analyzeResultShowNodes: ObjectProperty[List[Node]] = ObjectProperty(Nil)

  val analyzeResultNotNull: BooleanBinding =
    Bindings.createBooleanBinding(() => analyzeResultShowNodes().nonEmpty, analyzeResultShowNodes)

  analyzeResultShowNodes.onChange {
    analyzeResultContainer.children = analyzeResultShowNodes()
  }

  optHelpInfos.onChange {
    val infos: List[(HelpInfo, List[LogItem])] = optHelpInfos() match {
      case None => Nil
      case Some(Nil) => List((HelpInfo("未分析出可能原因", Some("https://www.qcloud.com/document/product/268/7752")), Nil))
      case Some(helps) => helps
    }
    val renderedHelps = infos.map { case (helpInfos, logs) =>
      (helpInfos, logs.map(log => (log, Analyzer.commentLog(log, selectedPlatform))))
    }.map { case (helpInfo, commentedLogs) =>
      Renderer.renderHelpInfo(helpInfo, commentedLogs)
    }
    analyzeResultShowNodes() = renderedHelps
  }

  // 问题列表
  val problems: List[Problem] = Analyzer.loadProblemList match {
    case Success(lst) => lst
    case Failure(thw) =>
      setError(thw.getMessage)
      Nil
  }
  val problemChoiceBox = new ChoiceBox(ObservableBuffer(problems.map(_.name)))
  problemChoiceBox.selectionModel().selectFirst()
  val problemNameToTag: Map[String, ProblemTag] = problems.map(problem => (problem.name, problem.tag)).toMap

  // 时间筛选
  val analyzeStartTimeTextField: DateTimeTextField = DateTimeTextField()
  val analyzeEndTimeTextField: DateTimeTextField = DateTimeTextField()

  val startAnalyzeButton = new Button {
    text = "开始分析"
    onAction = { _: ActionEvent =>
      val problemTag = problemNameToTag(problemChoiceBox.selectionModel().getSelectedItem)
      val startTime = analyzeStartTimeTextField.getTime
      val endTime = analyzeEndTimeTextField.getTime
      val filteredLogs = logs.Utils.timeFilter(startTime, endTime)(logList())
      Analyzer.analyzeLog(selectedPlatform)(filteredLogs, problemTag) match {
        case Success(resultList) =>
          val helpInfoList = resultList.map(res => (res.helpInfo, res.relatedLogs))
          optHelpInfos() = Some(helpInfoList)
        case Failure(thw) =>
          setError(thw.getMessage)
      }
    }
  }

  val clearAnalyzeResultButton = new Button {
    text = "清空帮助信息"
    onAction = { _: ActionEvent => optHelpInfos() = None }
  }

  // --------------- 主界面 ---------------------

  import UIHelper._

  val mainContainer = new BorderPane {
    top = vBoxContainer(
      flowContainer(baseLabel("运行平台"), platformChoiceBox, loadFileButton, fileHintLabel),
      flowContainer(
        baseLabel("问题描述"), problemChoiceBox,
        baseLabel("时间区间 从"), analyzeStartTimeTextField, baseLabel("至"), analyzeEndTimeTextField,
        startAnalyzeButton, clearAnalyzeResultButton
      )
    )
    center = new BorderPane {
      top = vBoxContainer(
        titleLabel("帮助信息").hideIf(createBooleanBinding(() => optHelpInfos().nonEmpty, optHelpInfos)),
        analyzeResultContainer,
        titleLabel("原始日志"),
        flowContainer(
          baseLabel("是否关键日志"), filterIsKeyLogChoiceBox,
          baseLabel("最低日志等级"), minLogLevelChoiceBox,
          baseLabel("日志信息"), filterMessageTextField,
          baseLabel("打印位置"), filterPositionTextField,
          isStrictModeCheckBox,
          includeUnknownCheckBox,
          baseLabel("筛选时间区间 从"), originLogStartTimeTextField, baseLabel("至"), originLogEndTimeTextField
        )
      )
      center = new ScrollPane {
        id = "log-scroll-pane"
        content = originalLogsContainer
      }
      bottom = errMessageText
    }
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "日志分析器"
    width = 1024
    height = 768
    minWidth = 400
    minHeight = 300
    scene = new Scene {
      content = mainContainer
      stylesheets = Seq(getClass.getResource("main.css").toExternalForm)
    }
  }
  mainContainer.prefWidth <== stage.width
  mainContainer.prefHeight <== stage.height - 22
}
