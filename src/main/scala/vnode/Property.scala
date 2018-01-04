package vnode

import org.scalajs.dom.Node
import org.scalajs.dom.html.Element

sealed trait Property

// node: Node, propertyName: String, previousValue: String
case class Hook(
    hook: (Node, String) => Unit,
    unhook: (Node, String) => Unit
) extends Property

case class Attributes(attributes: Map[String, String]) extends Property
case class Style(styles: Map[String, String]) extends Property
case class KeyValue(key: String, value: String) extends Property

object Property {

  //https://github.com/Matt-Esch/virtual-dom/blob/master/vdom/apply-properties.js

  def applyProperties(node: Element, props: Seq[Property]): Unit = {
    // previous版と一緒にまとめるとわかりづらくなるので分離させた

    props.foreach {
      case Hook(hook, _) =>
        hook.apply(node, "")
      case Attributes(attributes) =>
        attributes.foreach {
          case (attrName, attrValue) =>
            node.setAttribute(attrName, attrValue)
        }
      case Style(styles) =>
        styles.foreach {
          case (key, value) =>
            node.style.setProperty(key, value)
        }
    }
  }

  def applyProperties(node: Element, props: Seq[Property], previous: Seq[Property]): Unit = {

    // プロパティを全て取る

  }

  def removeProperty(node: Element, propName: String, props: Seq[Property], previous: Seq[Property]): Unit = {
    if (!previous.isEmpty) {

    }
  }
}