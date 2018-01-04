package vnode

import org.scalajs.dom.Node
import org.scalajs.dom.CharacterData

import scala.scalajs.js

sealed trait Patch

case class PNone(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PVText(vNode: VirtualTree, patch: VirtualText) extends Patch
case class PVNode(vNode: VirtualTree, patch: VirtualNode) extends Patch
case class PWidget(vNode: VirtualTree, patch: Widget) extends Patch
case class PProps(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class POrder(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PInsert(patch: VirtualTree) extends Patch
case class PRemove(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PThunk(vNode: VirtualTree, patch: VirtualTree) extends Patch

case class RenderOptions(

    patch: js.Object, // TODO
    render: (VirtualTree) => Node
)

object Patch {
  def patch(rootNode: Node, patches: Seq[Patch], renderOptions: Option[RenderOptions]): Node = {
    renderOptions

    null
  }

  def patchRecursive(rootNode: Node, patches: Seq[Patch], renderOptions: RenderOptions): Node = {

    null
  }

}

object PatchOp {
  def applyPatch(vPatch: Patch, domNode: Node, renderOptions: RenderOptions): Node = vPatch match {
    case PRemove(vNode, patch) =>
      removeNode(domNode, vNode)
      null // TODO

    case PInsert(patch) =>
      insertNode(domNode, patch, renderOptions)

    case PVText(_, patch) =>
      stringPatch(domNode, patch, renderOptions)
    case PWidget(vNode, patch) =>
      widgetPatch(domNode, vNode, patch, renderOptions)
    case PVNode(vNode, patch) =>
      vNodePatch(domNode, vNode, patch, renderOptions)
    case POrder(vNode, patch) =>
      null // TODO

    // TODO PROPS
    // TODO THUNK
    case _ => domNode
  }

  def removeNode(domNode: Node, vNode: VirtualTree): Unit = {

    if (domNode.parentNode != js.undefined) {
      domNode.parentNode.removeChild(domNode)
    }

    // TODO destory widget
  }

  def insertNode(parentNode: Node, vNode: VirtualTree, renderOptions: RenderOptions): Node = {
    val newNode = renderOptions.render(vNode)
    parentNode.appendChild(newNode)
    parentNode
  }

  def stringPatch(domNode: Node, vText: VirtualText, renderOptions: RenderOptions): Node = {
    // nodeType = 3 (TEXT_NODE)
    // https://developer.mozilla.org/ja/docs/Web/API/Node/nodeType
    if (domNode.nodeType == 3) {
      val textNode = domNode.asInstanceOf[CharacterData]
      textNode.replaceData(0, textNode.length, vText.text)
      textNode
    } else {
      val newNode = renderOptions.render(vText)
      domNode.parentNode.replaceChild(newNode, domNode)
      newNode
    }
  }

  def widgetPatch(domNode: Node, leftVNode: VirtualTree, widget: Widget, renderOptions: RenderOptions): Node = {
    def updateWidget(a: VirtualTree, b: Widget): Boolean = {
      // TODO
      true
    }

    val updating = updateWidget(leftVNode, widget)
    val newNode = if (updating) {
      widget.update(leftVNode, domNode)
    } else {
      renderOptions.render(widget)
    }

    if (domNode.parentNode != js.undefined && newNode != domNode) {
      domNode.parentNode.replaceChild(newNode, domNode)
    }

    if (!updating) {
      // TODO destroyWidget
    }

    newNode
  }

  def vNodePatch(domNode: Node, leftVNode: VirtualTree, vNode: VirtualNode, renderOptions: RenderOptions): Node = {
    val parentNode = domNode.parentNode
    val newNode = renderOptions.render(vNode)

    if (parentNode != js.undefined && newNode != domNode) {
      parentNode.replaceChild(newNode, domNode)
    }
    newNode
  }

  def reoderChildren(domNode: Node, moves: Any): Unit = {
    // TODO
  }

  def replaceRoot(oldRoot: Node, newRoot: Node): Node = {
    if (oldRoot != js.undefined && newRoot != js.undefined && oldRoot != newRoot && oldRoot.parentNode != js.undefined) {
      oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }
    newRoot
  }
}