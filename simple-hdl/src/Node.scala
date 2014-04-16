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
    for(i <- 0 until inputs.length){
      val input = inputs(i)
      if(input != null){
        Predef.assert(input.consumers.map(_._1).contains(this))
        for((consumer, index) <- input.consumers){
          if(consumer == this){
            Predef.assert(index == i)
          }
        }
      }
    }
    
    for((consumer, index) <- consumers){
      Predef.assert(consumer.inputs(index) == this)
    }

    Predef.assert(module != null)
    Predef.assert(module.nodes.contains(this))

    Predef.assert(emissionName == "")
  }
}
