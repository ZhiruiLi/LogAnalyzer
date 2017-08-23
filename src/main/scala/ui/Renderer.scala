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

    val dateFormatter = new SimpleDateFormat("MM-dd HH:mm:ss")

    def formatDate(date: Date): String = dateFormatter.format(date)

    def formatIsKey(isKeyLog: Boolean): String = if (isKeyLog) "*" else " "

    def formatLevel(lv: LogLevel): String = lv match {
      case LvVerbose => "V"
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

    def formatEquivocalLog(log: EquivocalLog): String = {
      def placeholder(tag: String) = s"<$tag: none>"
      s"${log.isKeyLog.map(formatIsKey).getOrElse(placeholder("关键日志"))}" +
        s"[${log.timestamp.map(formatDate).getOrElse(placeholder("打印时间"))}] " +
        s"${log.message.getOrElse(placeholder("日志信息"))} " +
        s"(${formatExt(log.extMessage)}) [${log.position.getOrElse(placeholder("打印位置"))}] " +
        s"[${log.level.map(formatLevel).getOrElse(placeholder("日志等级"))}]"
    }

    def formatUnknownLog(log: UnknownLog): String = {
      log.originalLog
    }
  }

  val levelColorMap: Map[LogLevel, Color] =
    Map(LvError -> Red, LvWarn -> Orange, LvInfo -> Black, LvDebug -> Black, LvVerbose -> DimGray)

  val defaultColor: Color = DarkGray

  def levelColor(lv: LogLevel): Color = levelColorMap.getOrElse(lv, defaultColor)

  def coloredText(txt: String, color: Color) = new Text {
    text = txt
    fill = color
    style = "-fx-font-family: Menlo, Consolas, Monospace;" +
            "-fx-font-size: 10pt;"
  }

  def renderRichLog(logItem: LogItem, comments: List[String]): Node = logItem match {
    case log@LegalLog(_, _, _, lv, _, _, _) =>
      val color = levelColor(lv)
      val logStr = Formatter.formatLegalLog(log)
      val text = s"${comments.mkString("\n")}\n  $logStr"
      coloredText(text, color)
    case log@EquivocalLog(originalLog, _, _, optLv, _, _, _) =>
      val color = optLv.map(levelColor).getOrElse(defaultColor)
      val logStr = Formatter.formatEquivocalLog(log)
      val text = s"${comments.mkString("\n")}\n $logStr\n  原始日志：$originalLog"
      coloredText(text, color)
    case log@UnknownLog(_) =>
      coloredText(Formatter.formatUnknownLog(log), defaultColor)
  }

  def renderHelpInfo(helpInfo: HelpInfo, richLogs: List[(LogItem, List[String])]): Node = {
    val lbHelpMsg = new HBox {
      children = Seq(
        new Label("可能原因："){ style = "-fx-font-size: 12pt;" },
        new TextFlow(new Text(helpInfo.message) { style = "-fx-font-size: 12pt;" })
      )
    }
    val optRenderedLink = helpInfo.helpPage
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
}
