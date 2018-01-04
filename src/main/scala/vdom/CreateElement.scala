package vdom

import vnode.{ Property, VirtualTree }
import org.scalajs.dom
import org.scalajs.dom.Node
import org.scalajs.dom.html.Document

object CreateElement {

  def createElement(vNode: VirtualTree, opt: Option[Document] = None): Node = {
    val doc = opt.getOrElse(dom.document)

    vNode match {
      case vnode.Widget(init, _, _) =>
        init()
      case vnode.VirtualText(text) =>
        doc.createTextNode(text)
      case x: vnode.VirtualNode =>

        val node = x.namespace match {
          case Some(namespace) =>
            doc.createElementNS(namespace, x.tagName)
          case None =>
            doc.createElement(x.tagName)
        }

        x.children
          .map(child => createElement(child, opt))
          .foreach(node.appendChild)

        // TODO apply propertie
        node
    }

  }

  def applyProperties(node: Node, properties: Seq[Property], prevProperties: Seq[Property]): Unit = {

    properties.foreach {
      case x @ vnode.Hook(hook, _) =>
      // TODO previous
      //hook.apply(node, Some(x))
    }
  }

  //def removeProperty
}
