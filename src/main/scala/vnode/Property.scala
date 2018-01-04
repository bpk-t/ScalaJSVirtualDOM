package vnode

import org.scalajs.dom.Node

sealed trait Property

// node: Node, propertyName: String, previousValue: String
case class Hook(
  hook: (Node, String) => Unit,
  unhook: (Node, String) => Unit) extends Property

case class Attributes(attributes: Map[String, String]) extends Property
case class Style(styles: Map[String, String]) extends Property
case class KeyValue(key: String, value: String) extends Property

object Property {

  //https://github.com/Matt-Esch/virtual-dom/blob/master/vdom/apply-properties.js

  def applyProperties(node: Node, props: Seq[Property], previous: Seq[Property]): Unit = {

  }

  def removeProperty(node: Node, propName: String, props: Seq[Property], previous: Seq[Property]): Unit = {
    if (!previous.isEmpty) {

    }
  }
}