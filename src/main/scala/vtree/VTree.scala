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

object VTree {

  def diff(a: VirtualTree, b: VirtualTree): Seq[Patch] = {

    Seq.empty
  }

  def walk(a: VirtualTree, b: Option[VirtualTree]): Seq[Patch] = {
    if (a == b) {
      return Seq.empty
    }

    (a, b) match {
      case (Thunk(_, _), Some(Thunk(_, _))) =>
      case (Thunk(_, _), _)                 =>
      case (_, Some(Thunk(_, _)))           =>

      case (_, None)                        =>

      case (x: VirtualNode, Some(y: VirtualNode)) =>
        if (x.tagName == y.tagName
          && x.namespace == y.namespace
          && x.key == y.key) {
          // TODO プロパティの変更チェック
          // TODO diffChildren
        } else {
          // VirtualNode同士で変更があった場合
          PVNode(a, y)
        }
      case (_, Some(y: VirtualNode)) =>
        // VirtualNode以外からVirtualNodeに変更された場合
        PVNode(a, y)

      case (x: VirtualText, Some(y: VirtualText)) if (x.text != y.text) =>
        PVText(a, y)
      case (_, Some(y: VirtualText)) =>
        PVText(a, y)

      case (x: Widget, Some(y: Widget)) =>
        PWidget(a, y)
      case (_, Some(y: Widget)) =>
        PWidget(a, y)
    }

    // TODO clearState

    Seq.empty
  }

  def diffChildren(a: VirtualNode, b: VirtualNode): Seq[Patch] = {

    // TODO reorder
    a.children.map(Some(_))
      .zipAll(b.children.map(Some(_)), None, None)
      .foreach {
        case (Some(leftNode), rightNode) =>
          walk(leftNode, rightNode)
        case (None, Some(rightNode)) =>
          PInsert(rightNode)
      }

    // TODO count ?
    // TODO orderSet

    Seq.empty
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