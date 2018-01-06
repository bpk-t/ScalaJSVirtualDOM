package vnode

import org.scalajs.dom.Node

sealed trait VirtualTree

case class VirtualNode(
    tagName: String,
    properties: Seq[Property] = Seq.empty,
    children: Seq[VirtualTree] = Seq.empty,
    key: Option[String] = None,
    namespace: Option[String] = None
) extends VirtualTree {

  private val virtualNodeChildren = children.filter(_.isInstanceOf[VirtualNode])
    .map(_.asInstanceOf[VirtualNode])

  // 子要素にWidgetを持っているか確認する
  val hasWidgets: Boolean = virtualNodeChildren.exists(_.hasWidgets) || children.exists(_.isInstanceOf[Widget])

  // 子要素にThunkを持っているか確認する
  val hasThunks: Boolean = virtualNodeChildren.exists(_.hasThunks) || children.exists(_.isInstanceOf[Thunk])

  val count: Int = children.length + virtualNodeChildren
    .map(_.count).sum
}

case class VirtualText(text: String) extends VirtualTree

case class Widget(
    init: () => Node,
    update: (VirtualTree, Node) => Node,
    destroy: (Node) => Unit
) extends VirtualTree

case class Thunk(
    vnode: VirtualTree,
    render: Option[VirtualTree] => VirtualTree
) extends VirtualTree

case class HandleThunkResult(
    a: VirtualTree,
    b: VirtualTree
)
object Thunk {
  def handleThunk(a: VirtualTree, b: VirtualTree): HandleThunkResult = {
    (a, b) match {
      case (aa: Thunk, bb: Thunk) =>
        HandleThunkResult(
          b = bb.render(Some(a)),
          a = aa.render(None)
        )
      case (aa, bb: Thunk) =>
        HandleThunkResult(
          a = aa,
          b = bb.render(Some(a))
        )
      case (aa: Thunk, bb) =>
        HandleThunkResult(
          b = bb,
          a = aa.render(None)
        )
      case (aa, bb) =>
        HandleThunkResult(
          a = aa,
          b = bb
        )
    }
  }
}