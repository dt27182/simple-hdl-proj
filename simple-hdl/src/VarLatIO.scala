package hdl
import scala.collection.mutable._
import IODirection._

object VarLatIO {
  def apply(name:String, reqWidth:Int, respWidth:Int, module:Module = Module.currentModule):VarLatIO = {
    return new VarLatIO(name,reqWidth, respWidth, module)
  }
}

class VarLatIO(_name:String, _reqDataWidth:Int, _respDataWidth:Int, _module:Module = Module.currentModule) {
  val req:DecoupledIO = DecoupledIO("req", OUTPUT, _reqDataWidth, _module)
  val resp:DecoupledIO = DecoupledIO("resp", INPUT, _respDataWidth, _module)
  val respPending = Bool("respPending", _module)
  var name = _name
}
