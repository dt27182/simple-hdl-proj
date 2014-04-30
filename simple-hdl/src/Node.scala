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
    //check that this node's inputs have this node in their consumer lists
    for(i <- 0 until inputs.length){
      val input = inputs(i)
      if(input != null){
        var foundConsumerPair = false
        for((consumer, index) <- input.consumers){
          if(consumer == this && index == i){
            foundConsumerPair = true
          }
        }
        Predef.assert(foundConsumerPair)
      }
    }
    
    //check that this node's consumers have this node as a input
    for((consumer, index) <- consumers){
      Predef.assert(consumer.inputs(index) == this)
    }

    //check that this node's consumers list does not have repeats
    val seenConsumers = new ArrayBuffer[(Node, Int)]
    for(consumerPair <- consumers){
      for((consumer, index) <- seenConsumers){
        Predef.assert(!(consumer == consumerPair._1 && index == consumerPair._2))
      }
      seenConsumers += consumerPair
    }
    
    //check module pointer
    Predef.assert(module != null)
    Predef.assert(module.nodes.contains(this))

    Predef.assert(emissionName == "")
  }
}
