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
  var gennedName = ""//base name with stuff for io access; this is different with different contexts
  var width = 0

  //methods
  def verify(): Unit
}
