package com.example.zhiruili.loganalyzer.ui

import scalafx.scene.control.{Control, Tooltip}

object Utils {

  def showTooltip(control: Control, tooltip: Tooltip, offsetX: Double = 0, offsetY: Double = 0): Unit = {
    val p = control.localToScene(0.0, 0.0)
    val win = control.getScene.getWindow
    tooltip.show(
      win,
      p.getX + control.getScene.getX + win.getX + offsetX,
      p.getY + control.getScene.getY + win.getY + control.height() + offsetY)
  }
}
