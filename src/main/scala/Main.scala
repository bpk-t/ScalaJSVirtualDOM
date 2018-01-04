import vnode.{ Attributes, VirtualNode, VirtualText }
import org.scalajs.dom.document
import vdom.CreateElement

object Main {
  def main(args: Array[String]): Unit = {

    val vnode = VirtualNode(
      tagName = "div",
      properties = Seq(
        Attributes(attributes = Map(
          "class" -> "test",
          "id" -> "div1"
        ))
      ),
      children = Seq(
        VirtualText("Hello World")
      )
    )
    val node = CreateElement.createElement(vnode)
    document.body.appendChild(node)
    println("hello")
  }
}
