package com.example.zhiruili.loganalyzer.ui

import java.text.SimpleDateFormat
import java.util.Date

import scalafx.Includes._
import scala.util.Try
import scalafx.scene.control.{TextField, Tooltip}
import scalafx.scene.input.KeyEvent
import scalafx.scene.text.Text

class DateTimeTextField(format: String,
                        doOnInputLegal: Option[() => Unit],
                        doOnInputIllegal: Option[() => Unit]) extends TextField {

  val tip = new Tooltip(s"格式不合法，输入格式：$format")
  tip.hide()

  def getDate: Option[Date] = {
    Try(dateTimeParser.parse(text().trim)).toOption
  }

  def getTime: Option[Long] = getDate.map(_.getTime)

  def isLegalInput: Boolean = text().trim match {
    case "" => true
    case s => Try(dateTimeParser.parse(s)).isSuccess
  }

  val measureWidth: Double = new Text(format).layoutBounds().getWidth

  prefWidth = measureWidth + 20

  val dateTimeParser = new SimpleDateFormat(format)

  promptText = format

  onKeyReleased = { _: KeyEvent =>
    (isLegalInput, tip.isShowing) match {
      case (true, true) =>
        tip.hide()
        doOnInputLegal.foreach(_())
      case (false, false) =>
        Utils.showTooltip(this, tip)
        doOnInputIllegal.foreach(_())
      case (true, _) =>
        doOnInputLegal.foreach(_())
      case (false, _) =>
        doOnInputIllegal.foreach(_())
    }
  }
}

object DateTimeTextField {

  def apply(format: String = "yy-MM-dd HH:mm:ss",
            doOnInputLegal: Option[() => Unit] = None,
            doOnInputIllegal: Option[() => Unit] = None): DateTimeTextField =

    new DateTimeTextField(format, doOnInputLegal, doOnInputIllegal)
}
