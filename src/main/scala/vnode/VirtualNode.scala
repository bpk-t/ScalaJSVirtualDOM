package vnode

import org.scalajs.dom.Node

sealed trait VirtualTree

case class VirtualNode(
  tagName: String,
  properties: Seq[Property] = Seq.empty,
  children: Seq[VirtualTree] = Seq.empty,
  key: Option[String] = None,
  namespace: Option[String] = None) extends VirtualTree {
  val count = children.length
}

case class VirtualText(text: String) extends VirtualTree

case class Widget(
  init: () => Node,
  update: (Node, Node) => Unit,
  destroy: (Node) => Unit) extends VirtualTree

case class Thunk(
  vnode: VirtualTree,
  render: VirtualTree => VirtualTree) extends VirtualTree