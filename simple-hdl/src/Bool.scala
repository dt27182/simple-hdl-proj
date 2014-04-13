package hdl
import scala.collection.mutable._

object Bool {
  def apply(name:String = "", module:Module = Module.currentModule) : Bool = {
    return new Bool(name,module)
  }
}

class Bool(_name:String = "", _module:Module = Module.currentModule) extends Wire(_name, 1, _module) {
}
