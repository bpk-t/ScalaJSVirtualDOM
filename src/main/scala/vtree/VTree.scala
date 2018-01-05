package vtree

import vnode._

// そのままコード書いてあとで書き直す

case class ReorderResult(
    children: Seq[VirtualTree],
    moves: Option[MoveResult]
)

case class MoveResult(
    removes: Seq[RemoveItem],
    inserts: Seq[InsertItem]
)

case class RemoveItem(
    from: Int,
    key: String
)

case class InsertItem(
    key: String,
    to: Int
)

case class DiffResult(
    rootVNode: VirtualTree,
    // key: VirtualDomの位置を表す
    patcheMap: Map[Int, Seq[Patch]]
)

object VTree {
  def diff(a: VirtualTree, b: VirtualTree): DiffResult = {
    DiffResult(
      rootVNode = a,
      patcheMap = walk(a, Some(b), 0)
    )
  }

  def walk(a: VirtualTree, b: Option[VirtualTree], index: Int): Map[Int, Seq[Patch]] = {
    if (b.fold(false)(_ == a)) {
      return Map.empty
    }

    (a, b) match {
      case (Thunk(_, _), Some(Thunk(_, _))) =>
        Map.empty // TODO
      case (Thunk(_, _), _) =>
        Map.empty // TODO
      case (_, Some(Thunk(_, _))) =>
        Map.empty // TODO
      case (_, None) =>
        Map(index -> Seq(PRemove(a)))
      case (x: VirtualNode, Some(y: VirtualNode)) =>
        if (x.tagName == y.tagName
          && x.namespace == y.namespace
          && x.key == y.key) {
          // TODO プロパティの変更チェック
          // TODO diffChildren

          diffChildren(x, y, index)
        } else {
          // VirtualNode同士で変更があった場合
          Map(index -> Seq(PVNode(a, y)))
        }
      case (_, Some(y: VirtualNode)) =>
        // VirtualNode以外からVirtualNodeに変更された場合
        Map(index -> Seq(PVNode(a, y)))

      case (x: VirtualText, Some(y: VirtualText)) if (x.text != y.text) =>

        Map(index -> Seq(PVText(a, y)))

      case (_, Some(y: VirtualText)) =>
        Map(index -> Seq(PVText(a, y)))

      case (x: Widget, Some(y: Widget)) =>
        Map(index -> Seq(PWidget(a, y)))

      case (_, Some(y: Widget)) =>
        Map(index -> Seq(PWidget(a, y)))
    }

    // TODO clearState
  }

  def diffChildren(a: VirtualNode, b: VirtualNode, index: Int): Map[Int, Seq[Patch]] = {

    // TODO immutable
    var result: Map[Int, Seq[Patch]] = Map.empty
    var mIndex = index

    // TODO reorder
    a.children.map(Some(_))
      .zipAll(b.children.map(Some(_)), None, None)
      .foreach { childPair =>
        mIndex = mIndex + 1
        childPair match {
          case (Some(leftNode), rightNode) =>
            result = result ++ walk(leftNode, rightNode, mIndex)
          case (None, Some(rightNode)) =>
            result = result + (mIndex -> Seq(PInsert(rightNode)))
        }

        if (childPair._1.isInstanceOf[VirtualNode]) {
          mIndex = mIndex + childPair._1.asInstanceOf[VirtualNode].count
        }
      }
    // TODO orderSet
    result
  }

  def reorder(aChildren: Seq[VirtualTree], bChildren: Seq[VirtualTree]): ReorderResult = {
    // 全ての子要素にKeyを持っていない場合は全部書き換え
    if (bChildren.forall(x => x.isInstanceOf[VirtualNode] && x.asInstanceOf[VirtualNode].key.isEmpty)) {
      return ReorderResult(
        bChildren,
        None
      )
    }

    // 全ての子要素にKeyを持っていない場合は全部書き換え
    if (aChildren.forall(x => x.isInstanceOf[VirtualNode] && x.asInstanceOf[VirtualNode].key.isEmpty)) {
      return ReorderResult(
        bChildren,
        None
      )
    }

    null
  }
}