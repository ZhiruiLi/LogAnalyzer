package ui

import com.example.zhiruili.loganalyzer._
import com.example.zhiruili.loganalyzer.logs._

import scala.io.Source
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



object AnalyzerApp extends JFXApp {

  // --------------- 原始日志信息 ------------------

  val originalLogsContainer = new VBox

  // 所有日志项
  val logList: ObjectProperty[List[LogItem]] = ObjectProperty(Nil)
  val logListNotNull: BooleanBinding = createBooleanBinding(() => logList().nonEmpty, logList)

  val errMessage: StringProperty = StringProperty("")
  val lbErrMessage = new Label {
    text <== errMessage
  }

  // 显示在 original log 区域的节点
  val originalLogShowNodes: ObjectProperty[List[Node]] = ObjectProperty(Nil)

  val defaultLabel = "请选择日志文件"
  val lbFileHint = Label(defaultLabel)
  val btnLoadFile = new Button {
    text = "导入本地日志文件"
    onAction = (_: ActionEvent) => {
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
    logList().map(log => (log, Analyzer.commentLog(log)))
  }, logList)

  // 渲染后的日志项
  val renderedLogs: ObjectBinding[List[(LogItem, Node)]] = createObjectBinding(() => {
    allLogsWithComment().map { case (log, optComment) => (log, Renderer.renderRichLog(log, optComment)) }
  }, allLogsWithComment)

  renderedLogs.onChange {
    updateOriginLogList()
  }

  originalLogShowNodes.onChange {
    originalLogsContainer.children = originalLogShowNodes()
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

  def updateOriginLogList(): Unit = {
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
    originalLogShowNodes() = newNodes
  }

  val filterChecker = Timeline(KeyFrame(500.ms, "", { _: ActionEvent =>
    if (filterIsUpdated()) {
      filterIsUpdated() = false
      updateOriginLogList()
    }
  }))
  filterChecker.setCycleCount(Timeline.Indefinite)
  filterChecker.play()

  // --------------- 帮助信息 ---------------------

  val analyzeResultContainer = new VBox

  // 帮助信息
  val helpInfos: ObjectProperty[List[(String, Option[String], List[LogItem])]] = ObjectProperty(Nil)

  val noHelpInfoLabel = Label("找不到问题的相关帮助")

  // 显示在 analyze result 区域的节点
  val analyzeResultShowNodes: ObjectProperty[List[Node]] = ObjectProperty(Nil)

  val analyzeResultNotNull: BooleanBinding =
    Bindings.createBooleanBinding(() => analyzeResultShowNodes().nonEmpty, analyzeResultShowNodes)

  analyzeResultShowNodes.onChange {
    analyzeResultContainer.children = analyzeResultShowNodes()
  }

  helpInfos.onChange {
    helpInfos() match {
      case Nil =>
        analyzeResultShowNodes() = List(noHelpInfoLabel)
      case helps =>
        val renderedHelps = helps.map { case (msg, optPage, logs) =>
          (msg, optPage, logs.map(log => (log, Analyzer.commentLog(log))))
        }.map { case (msg, optPage, commentedLogs) =>
          Renderer.renderHelpInfo(msg, optPage, commentedLogs)
        }
        analyzeResultShowNodes() = renderedHelps
    }
  }

  val choiceProblem = new ChoiceBox(ObservableBuffer("有声音无画面", "无法接收消息", "音频事件无回调", "首帧事件无回调"))
  choiceProblem.selectionModel().selectFirst()
  val choicePlatform = new ChoiceBox(ObservableBuffer("Android", "iOS"))
  choicePlatform.selectionModel().selectFirst()

  val platforms = Vector(PlatformAndroid, PlatformIOS)

  val btnStartAnalyze = new Button {
    text = "开始分析"
    onAction = { _: ActionEvent =>
      val platform = platforms(choicePlatform.selectionModel().getSelectedIndex)
      val problemCode = choiceProblem.selectionModel().getSelectedIndex
      println(s"platform: $platform, problem code: $problemCode")
      Analyzer.analyzeLog(platform)(logList(), problemCode) match {
        case Success(resultList) =>
          val helpInfoList = resultList.map { case (logItems, helpInfo) =>
            (helpInfo.message, helpInfo.helpPage, logItems)
          }
          helpInfos() = helpInfoList
        case Failure(thw) =>
          errMessage() = thw.getMessage
      }
    }
  }

  // --------------- 主界面 ---------------------

  val mainContainer = new BorderPane {
    top = new VBox {
      padding = Insets(top = 10, right = 20, left = 20, bottom = 10)
      spacing = 5
      children = Seq(
        lbFileHint,
        new FlowPane {
          hgap = 10
          children = Seq(
            btnLoadFile,
            Label("问题描述"), choiceProblem,
            Label("运行平台"), choicePlatform,
            btnStartAnalyze
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
            text = "分析结果"
            style = "-fx-font-size: 20pt"
          },
          analyzeResultContainer,
          new Label {
            text = "原始日志"
            style = "-fx-font-size: 20pt"
          },
          new FlowPane {
            hgap = 10
            children = Seq(
              Label("是否关键日志"), choiceIsKeyLog,
              Label("日志等级"), choiceLogLevel,
              Label("日志信息"), inputMessage,
              Label("打印位置"), inputPosition,
              cbIsRegex
            )
          }
        )
      }
      center = new ScrollPane {
        margin = Insets(top = 0, right = 0, left = 12, bottom = 0)
        padding = Insets(top = 0, right = 0, left = 5, bottom = 0)
        style = "-fx-background-color: transparent;" +
                "-fx-background: #FFFFFF;" +
                "-fx-border-color: #AAAAAA;"
        fitToWidth = true
        fitToHeight = true
        hbarPolicy = ScrollBarPolicy.AsNeeded
        vbarPolicy = ScrollBarPolicy.Always
        content = originalLogsContainer
      }
      bottom = lbErrMessage
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
