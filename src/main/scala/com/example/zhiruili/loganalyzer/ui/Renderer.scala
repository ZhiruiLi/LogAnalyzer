package com.example.zhiruili.loganalyzer.ui

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
import scalafx.scene.web.WebView

/**
  * 渲染相关
  */
object Renderer {

  /**
    * 日志格式化
    */
  object Formatter {

    val dateFormatter = new SimpleDateFormat("MM-dd HH:mm:ss")

    /**
      * 格式化日期
      *
      * @param date 日期
      * @return 格式化字符串
      */
    def formatDate(date: Date): String = dateFormatter.format(date)

    /**
      * 格式化关键路径日志标记
      *
      * @param isKeyLog 是否是关键路径日志
      * @return 格式化字符串
      */
    def formatIsKey(isKeyLog: Boolean): String = if (isKeyLog) "*" else " "

    /**
      * 格式化日志等级
      *
      * @param lv   日志等级
      * @return 格式化字符串
      */
    def formatLevel(lv: LogLevel): String = lv match {
      case LvVerbose => "V"
      case LvDebug => "D"
      case LvInfo => "I"
      case LvWarn => "W"
      case LvError => "E"
    }

    /**
      * 格式化额外信息
      *
      * @param ext  额外信息
      * @return 格式化字符串
      */
    def formatExt(ext: Map[String, String]): String = {
      ext.toList.map { case (k, v) => k + " -> " + v }.mkString(", ")
    }

    /**
      * 格式化合法日志
      *
      * @param log  合法日志
      * @return 格式化字符串
      */
    def formatLegalLog(log: LegalLog): String = {
      s"${formatIsKey(log.isKeyLog)}[${formatDate(log.timestamp)}] ${log.message} " +
        s"(${formatExt(log.extMessage)}) [${log.position}][${formatLevel(log.level)}]"
    }

    /**
      * 格式化模糊日志
      *
      * @param log  模糊日志
      * @return 格式化字符串
      */
    def formatEquivocalLog(log: EquivocalLog): String = {
      def placeholder(tag: String) = s"<$tag: none>"
      s"${log.isKeyLog.map(formatIsKey).getOrElse(placeholder("关键日志"))}" +
        s"[${log.timestamp.map(formatDate).getOrElse(placeholder("打印时间"))}] " +
        s"${log.message.getOrElse(placeholder("日志信息"))} " +
        s"(${formatExt(log.extMessage)}) [${log.position.getOrElse(placeholder("打印位置"))}] " +
        s"[${log.level.map(formatLevel).getOrElse(placeholder("日志等级"))}]"
    }

    /**
      * 格式化未知日志
      *
      * @param log  未知日志
      * @return 格式化字符串
      */
    def formatUnknownLog(log: UnknownLog): String = {
      s" ${log.originalLog}"
    }
  }

  // 映射日志等级和渲染颜色
  val levelColorMap: Map[LogLevel, Color] =
    Map(LvError -> Red, LvWarn -> Orange, LvInfo -> Black, LvDebug -> Black, LvVerbose -> DimGray)

  // 默认渲染颜色
  val defaultColor: Color = DarkGray

  /**
    * 通过日志等级获取渲染颜色
    *
    * @param lv   日志等级
    * @return 渲染颜色
    */
  def levelColor(lv: LogLevel): Color = levelColorMap.getOrElse(lv, defaultColor)

  /**
    * 将日志文字渲染为带颜色的节点
    *
    * @param logText      日志文本
    * @param color        颜色
    * @return Text 节点
    */
  def renderLog(logText: String, color: Color) = new Text {
    text = logText
    fill = color
    styleClass += "log"
  }

  /**
    * 渲染不显眼的日志信息
    *
    * @param logText    日志文本
    * @return Text 节点
    */
  def renderSilentLog(logText: String) = new Text {
    text = logText
    fill = defaultColor
    styleClass += "log-silent"
  }

  /**
    * 渲染带有注释的日志
    *
    * @param logItem    日志项
    * @param comments   注释列表
    * @return 渲染后的节点
    */
  def renderRichLog(logItem: LogItem, comments: List[String]): Node = logItem match {
    case log@LegalLog(_, _, _, lv, _, _, _) =>
      val color = levelColor(lv)
      val logStr = Formatter.formatLegalLog(log)
      val box = new VBox {
        children = comments.map(str => renderLog(str, color))
      }
      box.children += renderLog(logStr, color)
      box
    case log@EquivocalLog(_, _, _, optLv, _, _, _) =>
      val color = optLv.map(levelColor).getOrElse(defaultColor)
      val logStr = Formatter.formatEquivocalLog(log)
      val box = new VBox {
        children = comments.map(str => renderLog(str, color))
      }
      box.children += renderLog(logStr, color)
      box
    case log@UnknownLog(_) =>
      renderLog(Formatter.formatUnknownLog(log), defaultColor)
  }

  /**
    * 渲染帮助信息
    *
    * @param helpInfo   帮助信息
    * @param richLogs   相应的带有注释的日志列表
    * @return 渲染呢后的节点
    */
  def renderHelpInfo(helpInfo: HelpInfo, richLogs: List[(LogItem, List[String])]): Node = {
    def lb(text: String) = new Label(text) { styleClass += "text-emphatic" }
    def tf(text: String) = new TextFlow(new Text(text) { styleClass += "text-emphatic" })
    def link(text: String, link: String) = new Hyperlink(text) {
      onAction = { _: ActionEvent => Desktop.getDesktop.browse(new URI(link)) }
    }
    val lbHelpMsg = new HBox {
      children = Seq(lb("可能原因："), tf(helpInfo.message))
    }
    val optRenderedLink = helpInfo.helpPage
      .map(page => link(page, page))
      .map(link => new HBox {
        alignment = Pos.CenterLeft
        children = Seq(lb("帮助页面："), link)
      })
    val renderedLogs = richLogs.map { case (log, optCom) => renderRichLog(log, optCom) }
    val tailNodes = {
      val relatedLogNodes = renderedLogs match {
        case Nil => Nil
        case _ => List(new VBox {
          children = lb("相关日志：")::renderedLogs
        })
      }
      optRenderedLink.map(_::relatedLogNodes).getOrElse(relatedLogNodes)
    }
    new VBox {
      spacing = 5
      children = lbHelpMsg::tailNodes
    }
  }

  case class RichLog(item: LogItem, comments: List[String])
  case class RenderedLog(item: LogItem, htmlString: String)

  def renderRichLogToHtml(log: RichLog): String = {
    s"""<p><font>${(log.item.toString::log.comments).mkString("<br/>")}</font></p>"""
  }

  def renderLogPanel(renderedLogs: List[RenderedLog]): Node = {
    val webView = new WebView
    webView.engine.loadContent(s"""<body><div>${renderedLogs.map(_.htmlString).mkString("<br/>")}</div></body>""")
    webView.onclick
    webView
  }
}
