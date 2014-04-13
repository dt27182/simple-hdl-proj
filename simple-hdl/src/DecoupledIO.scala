package hdl
import scala.collection.mutable._

object Direction extends Enumeration {
  type Direction = Value
  val INPUT, OUTPUT = Value
}

import Direction._

object DecoupledIO {
  def apply(name:String, dir: Direction, dataWidth: Int, module:Module = Module.currentModule):DecoupledIO = {
    return new DecoupledIO(name, dir, dataWidth, module)
  }
}

class DecoupledIO(_name:String, _dir: Direction, _dataWidth: Int, _module:Module = Module.currentModule) {
  //child nodes
  var ready:Bool = Bool("ready", _module)
  var valid:Bool = Bool("valid", _module)
  var bits:Wire = Wire("bits", _dataWidth, _module)
  
  var dir:Direction = _dir
  
  //chisel code gen info
  var name = _name
}
