import vnode.VirtualNode
import org.scalajs.dom.document
import vdom.CreateElement

object Main {
  def main(args: Array[String]): Unit = {

    val vnode = VirtualNode("div")
    val node = CreateElement.createElement(vnode)
    document.body.appendChild(node)
    println("hello")
  }
}
