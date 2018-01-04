package vnode

import org.scalajs.dom.Node
import org.scalajs.dom.CharacterData

import scala.scalajs.js

sealed trait Patch

case class PNone(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PVText(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PVNode(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PWidget(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PProps(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class POrder(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PInsert(patch: VirtualTree) extends Patch
case class PRemove(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PThunk(vNode: VirtualTree, patch: VirtualTree) extends Patch

case class RenderOptions(

  patch: js.Object, // TODO
  render: (VirtualTree) => Node)

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
  /*
  def applyPatch(vPatch: Patch, domNode: Node, renderOptions: js.Object): Node = vPatch match {
  }
  */

  def removeNode(domNode: Node, vNode: VirtualNode): Unit = {

    if (domNode.parentNode != js.undefined) {
      domNode.parentNode.removeChild(domNode)
    }

    // TODO destory widget
  }

  def insertNode(parentNode: Node, vNode: VirtualNode, renderOptions: RenderOptions): Node = {
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
}