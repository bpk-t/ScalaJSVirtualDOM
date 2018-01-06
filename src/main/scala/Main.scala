import vnode.{ Attributes, Patch, VirtualNode, VirtualText }
import org.scalajs.dom.document
import vdom.CreateElement
import vtree.VTree

object Main {
  def main(args: Array[String]): Unit = {
    var count = 0
    var tree = render(count)
    var rootNode = CreateElement.createElement(tree)
    document.body.appendChild(rootNode)

    org.scalajs.dom.window.setInterval(() => {
      count = count + 1
      val newTree = render(count)
      val patches = VTree.diff(tree, newTree)
      rootNode = Patch.patch(rootNode, patches)
      tree = newTree

    }, 1000)
    println("hello")
  }

  def render(count: Int): VirtualNode = {
    VirtualNode(
      tagName = "div",
      properties = Seq(
        Attributes(attributes = Map(
          "class" -> s"test${count}",
          "id" -> s"div${count}"
        ))
      ),
      children = Seq(
        VirtualNode(
          tagName = "div",
          children = Seq(
            VirtualText(s"Hello World count = $count")
          )
        ),
        VirtualNode(
          tagName = "div",
          children = Seq(
            VirtualText(s"Hello World count = ${count + 1}")
          )
        )
      )
    )
  }
}
