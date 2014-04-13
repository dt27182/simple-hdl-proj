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
    Predef.assert(inputs.length == 1)
  }
}
