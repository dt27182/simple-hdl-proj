package hdl
import scala.collection.mutable._

object IODirection extends Enumeration {
  type IODirection = Value
  val INPUT, OUTPUT = Value
}

import IODirection._

abstract class IOCollection

object DecoupledIO {
  def apply(name:String, dir: IODirection, dataWidth: Int, module:Module = Module.currentModule):DecoupledIO = {
    return new DecoupledIO(name, dir, dataWidth, module)
  }
}

class DecoupledIO(_name:String, _dir: IODirection, _dataWidth: Int, _module:Module = Module.currentModule) extends IOCollection{
  //child nodes
  var ready:Bool = Bool("ready", _module)
  var valid:Bool = Bool("valid", _module)
  var bits:Wire = Wire("bits", _dataWidth, _module)
  
  var dir:IODirection = _dir
  
  //chisel code gen info
  var name = _name
}
