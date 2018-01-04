package vdom

import vnode.{ Property, VirtualTree }
import org.scalajs.dom
import org.scalajs.dom.Node
import org.scalajs.dom.html.Document
import org.scalajs.dom.html.Element

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
        // プロパティを設定
        Property.applyProperties(node.asInstanceOf[Element], x.properties)

        // 子要素の作成
        x.children
          .map(child => createElement(child, opt))
          .foreach(node.appendChild)
        node
    }
  }
}
