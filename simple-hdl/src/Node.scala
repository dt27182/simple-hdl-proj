package hdl
import scala.collection.mutable._

abstract class Node {
  //connections to other nodes
  val inputs = new ArrayBuffer[Node]
  val consumers = new ArrayBuffer[(Node, Int)]
  //connections to enclosing module
  var module:Module = null
  //chisel code gen info
  var name = ""//base name of node
  var emissionName = ""//name used for code gen; should be "" until set by the code gen method
  var width = 0

  //methods

  def verify(): Unit = {
    Predef.assert(emissionName == "")
  }
}
