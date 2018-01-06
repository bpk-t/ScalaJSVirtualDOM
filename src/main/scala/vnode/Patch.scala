package vnode

import org.scalajs.dom.Node
import org.scalajs.dom.CharacterData
import vdom.CreateElement
import vtree.DiffResult

import scala.scalajs.js

sealed trait Patch

case class PNone(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PVText(vNode: VirtualTree, patch: VirtualText) extends Patch
case class PVNode(vNode: VirtualTree, patch: VirtualNode) extends Patch
case class PWidget(vNode: VirtualTree, patch: Widget) extends Patch
case class PProps(vNode: VirtualTree, patch: Seq[Property]) extends Patch
case class POrder(vNode: VirtualTree, patch: VirtualTree) extends Patch
case class PInsert(patch: VirtualTree) extends Patch
case class PRemove(vNode: VirtualTree) extends Patch
case class PThunk(patch: DiffResult) extends Patch

case class RenderOptions(
    patch: (Node, DiffResult, RenderOptions) => Node,
    render: (VirtualTree) => Node
)

object Patch {
  def patch(rootNode: Node, patches: DiffResult, renderOptionsOpt: Option[RenderOptions] = None): Node = {
    val renderOption = renderOptionsOpt.getOrElse(RenderOptions(
      patch = patchRecursive,
      render = (tree) => CreateElement.createElement(tree)
    ))
    renderOption.patch(rootNode, patches, renderOption)
  }

  def patchRecursive(rootNode: Node, patches: DiffResult, renderOptions: RenderOptions): Node = {
    if (patches.patcheMap.isEmpty) {
      // パッチが存在しない場合は変更せずにそのまま返す
      return rootNode
    } else {
      val indices = patches.patcheMap.map(_._1).toSeq
      val index = domIndex(rootNode, patches.rootVNode, indices)

      // nodeの操作
      var mRootNode: Node = rootNode
      indices.foreach { nodeIndex =>
        mRootNode = applyPatch(
          rootNode,
          index(nodeIndex),
          patches.patcheMap(nodeIndex),
          renderOptions
        )
      }
      mRootNode
    }
  }

  def applyPatch(rootNode: Node, domNode: Node, patches: Seq[Patch], renderOptions: RenderOptions): Node = {
    if (domNode == js.undefined) {
      return rootNode
    }

    var result: Node = rootNode
    patches.foreach { patch =>
      val newNode = PatchOp.applyPatch(patch, domNode, renderOptions)
      if (domNode == rootNode) {
        result = newNode
      }
    }
    result
  }

  def domIndex(rootNode: Node, tree: VirtualTree, indices: Seq[Int]): Map[Int, Node] = {
    def indexInRange(indices: Seq[Int], left: Int, right: Int): Boolean = {
      // 2分探索
      if (indices.isEmpty) {
        return false
      }

      var minIndex = 0
      var maxIndex = indices.length - 1
      var currentIndex: Int = 0
      var currentItem: Int = 0

      while (minIndex <= maxIndex) {
        currentIndex = Math.floor((maxIndex + minIndex) / 2).toInt
        currentItem = indices(currentIndex)

        if (minIndex == maxIndex) {
          return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
          minIndex = currentIndex + 1
        } else if (currentItem > right) {
          maxIndex = currentIndex - 1
        } else {
          return true
        }
      }
      return false
    }
    def recurse(rootNode: Option[Node], tree: VirtualTree, indices: Seq[Int], rootIndex: Int): Map[Int, Node] = {
      // TODO immutable
      var result: Map[Int, Node] = Map.empty
      if (rootNode.isDefined) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
          result = result + (rootIndex -> rootNode.get)
        }

        if (tree.isInstanceOf[VirtualNode]) {
          val vNodeTree = tree.asInstanceOf[VirtualNode]
          var mRootIndex = rootIndex
          vNodeTree.children.zipWithIndex.foreach {
            case (child, index) =>
              mRootIndex = mRootIndex + 1
              val nextIndex = mRootIndex + (if (child.isInstanceOf[VirtualNode]) {
                child.asInstanceOf[VirtualNode].count
              } else {
                0
              })

              if (indexInRange(indices, mRootIndex, nextIndex)) {
                result = result ++ recurse(rootNode.flatMap(x => Option(x.childNodes(index))), child, indices, mRootIndex)
              }
              mRootIndex = nextIndex
          }
        }
      }
      result
    }

    if (indices.isEmpty) {
      Map.empty
    } else {
      recurse(Option(rootNode), tree, indices.sorted, 0)
    }
  }

}

object PatchOp {
  def applyPatch(vPatch: Patch, domNode: Node, renderOptions: RenderOptions): Node = vPatch match {
    case PRemove(vNode) =>
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

  def reorderChildren(domNode: Node, moves: Any): Unit = {
    // TODO
  }

  def replaceRoot(oldRoot: Node, newRoot: Node): Node = {
    if (oldRoot != js.undefined && newRoot != js.undefined && oldRoot != newRoot && oldRoot.parentNode != js.undefined) {
      oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }
    newRoot
  }
}