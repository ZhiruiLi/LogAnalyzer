package ui

import scalafx.Includes._
import java.awt.Desktop
import java.net.URI
import java.text.SimpleDateFormat
import java.util.Date

import com.example.zhiruili.loganalyzer.logs._

import scalafx.event.ActionEvent
import scalafx.geometry.Pos
import scalafx.scene.Node
import scalafx.scene.control.{Hyperlink, Label}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.text.{Font, Text, TextFlow}

object Renderer {

  object Formatter {

    val dateFormatter = new SimpleDateFormat("yy-MM-dd HH:mm:ss")

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
    style = "-fx-font-family: Menlo, Consolas, Monospace;" +
            "-fx-font-size: 11pt;"
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

  def renderHelpInfo(helpMessage: String, optHelpPage: Option[String], richLogs: List[(LogItem, Option[String])]): Node = {
    val lbHelpMsg = new TextFlow(new Text(s"帮助信息：$helpMessage"))
    val optRenderedLink = optHelpPage
      .map(page => new Hyperlink(page) {
        alignment = Pos.CenterLeft
        onAction = { _: ActionEvent =>
          Desktop.getDesktop.browse(new URI(page))
        }
      })
      .map(link => new HBox {
        alignment = Pos.CenterLeft
        children = Seq(new TextFlow(new Text("帮助页面：更多信息请参考 —— ")), link)
      })
    val renderedLogs = richLogs.map { case (log, optCom) => renderRichLog(log, optCom) }
    val tailNodes = optRenderedLink.map(_::renderedLogs).getOrElse(renderedLogs)
    new VBox {
      children = lbHelpMsg::tailNodes
    }
  }
}
