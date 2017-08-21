package ui

import scalafx.Includes._
import java.awt.Desktop
import java.net.URI
import java.text.SimpleDateFormat
import java.util.Date

import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.HelpInfo
import com.example.zhiruili.loganalyzer.logs._

import scalafx.event.ActionEvent
import scalafx.geometry.Pos
import scalafx.scene.Node
import scalafx.scene.control.{Hyperlink, Label}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.text.{Text, TextFlow}

object Renderer {

  object Formatter {

    val dateFormatter = new SimpleDateFormat("yy-MM-dd HH:mm:ss")

    def formatDate(date: Date): String = dateFormatter.format(date)

    def formatIsKey(isKeyLog: Boolean): String = if (isKeyLog) "*" else " "

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
    style = "-fx-font-family: Menlo, Consolas, Monospace;" +
            "-fx-font-size: 10pt;"
  }

  def renderRichLog(logItem: LogItem, optComment: Option[String]): Node = logItem match {
    case log@LegalLog(_, _, lv, _, _, _) =>
      val color = levelColor(lv)
      val logStr = Formatter.formatLegalLog(log)
      val text = optComment.map(comment => s"$comment\n  $logStr").getOrElse(s"  $logStr")
      coloredText(text, color)
    case log@UnknownLog(_) =>
      coloredText(Formatter.formatUnknownLog(log), defaultColor)
  }

  def renderHelpInfo(helpMessage: String, optHelpPage: Option[String], richLogs: List[(LogItem, Option[String])]): Node = {
    val lbHelpMsg = new HBox {
      children = Seq(
        new Label("可能原因："){ style = "-fx-font-size: 12pt;" },
        new TextFlow(new Text(helpMessage) { style = "-fx-font-size: 12pt;" })
      )
    }
    val optRenderedLink = optHelpPage
      .map(page => new Hyperlink(page) {
        onAction = { _: ActionEvent => Desktop.getDesktop.browse(new URI(page)) }
      })
      .map(link => new HBox {
        alignment = Pos.CenterLeft
        children = Seq(
          new Label("帮助页面：") {
            style = "-fx-font-size: 12pt;"
          }, link)
      })
    val renderedLogs = richLogs.map { case (log, optCom) => renderRichLog(log, optCom) }
    val tailNodes = {
      val relatedLogNodes = renderedLogs match {
        case Nil => Nil
        case _ => List(new VBox {
          children = Seq(
            new Label("相关日志：") { style = "-fx-font-size: 12pt;" },
            new VBox { children = renderedLogs })
        })
      }
      optRenderedLink.map(_::relatedLogNodes).getOrElse(relatedLogNodes)
    }
    new VBox {
      spacing = 5
      children = lbHelpMsg::tailNodes
    }
  }

  def renderHelpInfos(helpInfos: List[(List[(LogItem, Option[String])], HelpInfo)]): Node = {
    ???
  }
}
