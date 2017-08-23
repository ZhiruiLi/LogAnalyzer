package ui

import com.example.zhiruili.loganalyzer._
import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.{HelpInfo, Problem, ProblemTag}
import com.example.zhiruili.loganalyzer.logs._
import com.example.zhiruili.utils.Utils._

import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.beans.binding.{Bindings, BooleanBinding, ObjectBinding}
import scalafx.beans.property.{BooleanProperty, ObjectProperty, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.{Node, Scene}
import scalafx.scene.layout.{BorderPane, FlowPane, VBox}
import scalafx.stage.FileChooser
import scalafx.event.ActionEvent
import scalafx.scene.control.ScrollPane.ScrollBarPolicy
import scalafx.scene.control._
import scalafx.scene.input.KeyEvent
import scalafx.scene.text.{Text, TextFlow}

object AnalyzerApp extends JFXApp {

  object Implicits {
    implicit val logItemPairAsLogItem: ((LogItem, Node)) => LogItem = _._1
  }

  import Implicits._

  // --------------- 公用 ------------------------

  val cbFilterPlatform = new ChoiceBox(ObservableBuffer("Android", "iOS"))
  cbFilterPlatform.selectionModel().selectFirst()
  val platforms = Vector(PlatformAndroid, PlatformIOS)

  def currentPlatform: Platform = platforms(cbFilterPlatform.selectionModel().getSelectedIndex)

  // --------------- 原始日志信息 ------------------

  val originalLogsContainer = new VBox

  // 所有日志项
  val logList: ObjectProperty[List[LogItem]] = ObjectProperty(Nil)
  val logListNotNull: BooleanBinding = createBooleanBinding(() => logList().nonEmpty, logList)

  // 错误信息
  val errMessage: StringProperty = StringProperty("")
  val errMessageNode = new TextFlow(
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

  val defaultLabel = "请选择日志文件"
  val lbFileHint = Label(defaultLabel)
  val btnLoadFile = new Button {
    text = "导入本地日志文件"
    onAction = (_: ActionEvent) => {
      clearError()
      val chooser = new FileChooser
      val selectedFile = chooser.showOpenDialog(stage)
      if (selectedFile != null) {
        println(selectedFile)
        Try(Source.fromFile(selectedFile).mkString).flatMap(Analyzer.logParser.parseString) match {
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
  val allLogsWithComment: ObjectBinding[List[(LogItem, List[String])]] = createObjectBinding(() => {
    logList().map(log => (log, Analyzer.commentLog(log, currentPlatform)))
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
  val cbFilterIsKeyLog = new ChoiceBox(ObservableBuffer("无约束", "是", "否"))
  cbFilterIsKeyLog.selectionModel().selectFirst()
  cbFilterIsKeyLog.selectionModel().selectedIndexProperty().onChange {
    setFilterExpired()
  }
  val filterOptIsKeyLog: Option[Boolean] = cbFilterIsKeyLog.selectionModel().getSelectedIndex match {
    case 0 => None
    case 1 => Some(true)
    case 2 => Some(false)
  }

  // 过滤最低等级日志
  val cbMinLogLevel = new ChoiceBox(ObservableBuffer("Verbose", "Debug", "Info", "Warn", "Error"))
  cbMinLogLevel.selectionModel().selectFirst()
  cbMinLogLevel.selectionModel().selectedIndexProperty().onChange {
    setFilterExpired()
  }
  def filterMinLogLevel: LogLevel = cbMinLogLevel.selectionModel().getSelectedIndex match {
    case 0 => LvVerbose
    case 1 => LvDebug
    case 2 => LvInfo
    case 3 => LvWarn
    case 4 => LvError
  }

  // 日志信息过滤输入框
  val tfFilterMessage: TextField = new TextField { onKeyTyped = { _: KeyEvent => setFilterExpired() } }
  def filterMessage: Option[String] = tfFilterMessage.text() match {
    case "" => None
    case str => Some(if (isStrictMode) str else createFuzzyRegex(str))
  }

  // 日志打印位置过滤输入框
  val tfFilterPosition: TextField = new TextField { onKeyTyped = { _: KeyEvent => setFilterExpired() } }
  def filterPosition: Option[String] = tfFilterPosition.text() match {
    case "" => None
    case str => Some(if (isStrictMode) str else createFuzzyRegex(str))
  }

  // 是否采用正则匹配，未选择则为模糊匹配
  val cbIsStrictMode = new CheckBox("严格匹配(正则)") { onAction = { _: ActionEvent => setFilterExpired() } }
  def isStrictMode: Boolean = cbIsStrictMode.selected()

  // 输出结果是否包含无法解析的日志
  val cbIncludeUnknown = new CheckBox("结果包含无法解析的日志") { onAction = { _: ActionEvent => setFilterExpired() } }

  // 过滤时间区间
  val inputOriginLogStartTime: DateTimeTextField = DateTimeTextField(doOnInputLegal = Some(() => setFilterExpired()))
  val inputOriginLogEndTime: DateTimeTextField = DateTimeTextField(doOnInputLegal = Some(() => setFilterExpired()))

  def updateOriginLogList(): Unit = {
    val includeUnknown = cbIncludeUnknown.selected()
    val logsAfterFilter =
      logs.Utils.timeFilter(inputOriginLogStartTime.getTime, inputOriginLogEndTime.getTime)(renderedLogs())
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
      (helpInfos, logs.map(log => (log, Analyzer.commentLog(log, currentPlatform))))
    }.map { case (helpInfo, commentedLogs) =>
      Renderer.renderHelpInfo(helpInfo, commentedLogs)
    }
    analyzeResultShowNodes() = renderedHelps
  }

  // 问题列表
  val problems: List[Problem] = Analyzer.loadProblemList
  val choiceProblem = new ChoiceBox(ObservableBuffer(problems.map(_.name)))
  choiceProblem.selectionModel().selectFirst()
  val problemNameToTag: Map[String, ProblemTag] = problems.map(problem => (problem.name, problem.tag)).toMap

  // 时间筛选
  val inputAnalyzeStartTime: DateTimeTextField = DateTimeTextField()
  val inputAnalyzeEndTime: DateTimeTextField = DateTimeTextField()

  val btnStartAnalyze = new Button {
    text = "开始分析"
    onAction = { _: ActionEvent =>
      val problemTag = problemNameToTag(choiceProblem.selectionModel().getSelectedItem)
      val startTime = inputAnalyzeStartTime.getTime
      val endTime = inputAnalyzeEndTime.getTime
      val filteredLogs = logs.Utils.timeFilter(startTime, endTime)(logList())
      Analyzer.analyzeLog(currentPlatform)(filteredLogs, problemTag) match {
        case Success(resultList) =>
          val helpInfoList = resultList.map(res => (res.helpInfo, res.relatedLogs))
          optHelpInfos() = Some(helpInfoList)
        case Failure(thw) =>
          setError(thw.getMessage)
      }
    }
  }

  val btnClearAnalyzeResult = new Button {
    text = "清空帮助信息"
    onAction = { _: ActionEvent => optHelpInfos() = None }
  }

  // --------------- 主界面 ---------------------

  val mainContainer = new BorderPane {
    top = new VBox {
      padding = Insets(top = 10, right = 20, left = 20, bottom = 10)
      spacing = 5
      children = Seq(
        new FlowPane {
          hgap = 10
          children = Seq(Label("运行平台"), cbFilterPlatform, btnLoadFile, lbFileHint)
        },
        new FlowPane {
          hgap = 10
          children = Seq(
            Label("问题描述"), choiceProblem,
            Label("时间区间 从"), inputAnalyzeStartTime, Label("至"), inputAnalyzeEndTime,
            btnStartAnalyze, btnClearAnalyzeResult
          )
        }
      )
    }
    center = new BorderPane {
      top = new VBox {
        padding = Insets(top = 0, right = 20, left = 20, bottom = 10)
        spacing = 10
        children = Seq(
          new Label {
            text = "帮助信息"
            managed <== createBooleanBinding(() => optHelpInfos().nonEmpty, optHelpInfos)
            visible <== managed
            style = "-fx-font-size: 18pt"
          },
          analyzeResultContainer,
          new Label {
            text = "原始日志"
            style = "-fx-font-size: 18pt"
          },
          new FlowPane {
            hgap = 10
            vgap = 5
            children = Seq(
              Label("是否关键日志"), cbFilterIsKeyLog,
              Label("最低日志等级"), cbMinLogLevel,
              Label("日志信息"), tfFilterMessage,
              Label("打印位置"), tfFilterPosition,
              cbIsStrictMode,
              cbIncludeUnknown,
              Label("筛选时间区间 从"), inputOriginLogStartTime, Label("至"), inputOriginLogEndTime
            )
          }
        )
      }
      center = new ScrollPane {
        margin = Insets(top = 0, right = 0, left = 0, bottom = 0)
        padding = Insets(top = 0, right = 0, left = 10, bottom = 0)
        style = "-fx-background-color: transparent;" +
                "-fx-background: #FFFFFF;" +
                "-fx-border-color: #FFFFFF;"
        fitToWidth = true
        fitToHeight = true
        hbarPolicy = ScrollBarPolicy.AsNeeded
        vbarPolicy = ScrollBarPolicy.Always
        content = originalLogsContainer
      }
      bottom = errMessageNode
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
    }
  }
  mainContainer.prefWidth <== stage.width
  mainContainer.prefHeight <== stage.height - 22
}
