package FSM 
 
import hdl._ 
import scala.collection.mutable._
import Direction._

class DUT(par: Module = Module.currentModule, name: String = "") extends Module(par, name) {
  //must have this at the beginning of the module declaration
  Module.currentModule = this
  
  //actual code goes here
  
  
  
  //must have these at the end of the module declaration
  Module.currentModule = parent
}

class FSM(par: Module = Module.currentModule, name: String = "") extends Module(par, name) {
  //must have this at the begining of the module declaration
  Module.currentModule = this
  //actual code goes here
  val in0 = Wire(name ="in0", width = 32)
  val in1 = Wire(name = "in1", width = 32)
  val out = Wire(name = "out", width = 32)
  val mem_cmd = DecoupledIO("mem_cmd", OUTPUT, 32)
  val mem_resp = DecoupledIO("mem_resp", INPUT, 32)
  val bool_out = Bool(name = "bool_out")
  decoupledIOs += mem_cmd
  decoupledIOs += mem_resp
  addInput(in0)
  addInput(in1)
  addOutput(out)
  addOutput(bool_out)
  val a = Plus(in0, in1, "a")
  Assign(a, out)
  Assign(mem_resp.bits, mem_cmd.bits)
  Assign(mem_resp.valid, mem_cmd.valid)
  Assign(mem_cmd.ready, mem_resp.ready)
  val b = Not(mem_cmd.ready, "b")
  Assign(b, bool_out)
  val c = BitConst(3, 32, "c")
  val d = BoolConst(true, "d")
  //val e_updates = new ArrayBuffer[(Bool, Bool)]
  //e_updates += ((d, d))
  //val e = Reg(updates = e_updates, name = "e")
  //must have these at the end of the module declaration 
  Module.currentModule = parent
}

object test {
  def main(args: Array[String]): Unit = {
    hdlMain(new FSM, "/home/eecs/wenyu/multithread-transform/simple-hdl-proj/generated/fsm.scala", "FSM")
  }
}
