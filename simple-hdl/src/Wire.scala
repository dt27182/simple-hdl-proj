package hdl
import scala.collection.mutable._

object Wire {
  def apply(name:String = "", width:Int = 0, module:Module = Module.currentModule): Wire = {
    return new Wire(name, width, module)
  }
}

class Wire (_name:String = "", _width:Int = 0, _module:Module = Module.currentModule) extends Node {
  name = _name
  width = _width
  module = _module
  module.nodes += this

  override def verify() : Unit = {
    super.verify
    Predef.assert(inputs.length <= 1)
    if(inputs.length == 1){
      Predef.assert(inputs(0).isInstanceOf[Op])
    }
    for((consumer, index) <- consumers){
      Predef.assert(consumer.isInstanceOf[Op])
    }
  }

  def getReg: Reg = inputs(0).asInstanceOf[RegRead].superOp.asInstanceOf[Reg]
}
