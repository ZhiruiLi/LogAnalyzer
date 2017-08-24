package com.example.zhiruili.loganalyzer.ui

import javafx.collections.ObservableList

import scalafx.Includes._
import scalafx.beans.binding.BooleanBinding
import scalafx.beans.property.StringProperty
import scalafx.scene.Node
import scalafx.scene.control.Label
import scalafx.scene.layout.{FlowPane, VBox}

object UIHelper {

  def vBoxContainer(subs: ObservableList[Node]): VBox = new VBox {
    styleClass += "container-box"
    children = subs
  }

  def vBoxContainer(subs: Node*): VBox = new VBox {
    styleClass += "container-box"
    children = subs
  }

  def flowContainer(subs: ObservableList[Node]): FlowPane = new FlowPane {
    styleClass += "container-flow"
    children = subs
  }

  def flowContainer(subs: Node*): FlowPane = new FlowPane {
    styleClass += "container-flow"
    children = subs
  }

  def baseLabel(txt: StringProperty): Label = new Label {
    styleClass += "label-base"
    text <== txt
  }

  def baseLabel(txt: String): Label = new Label {
    styleClass += "label-base"
    text = txt
  }

  def titleLabel(txt: StringProperty): Label = new Label {
    styleClass += "label-title"
    text <== txt
  }

  def titleLabel(txt: String): Label = new Label {
    styleClass += "label-title"
    text = txt
  }

  implicit class RichNode(node: Node) {

    def hideIf(boolProp: BooleanBinding): Node = {
      node.managed <== boolProp
      node.visible <== node.managed
      node
    }

    def withId(id: String): Node = {
      node.id = id
      node
    }

    def withStyleClasses(styleClasses: String*): Node = {
      node.styleClass = styleClasses
      node
    }
  }
}
