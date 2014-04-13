package FSM 
 
import hdl._ 
import scala.collection.mutable.HashMap
import Direction._

class FSM(par: Module = Module.currentModule, name: String = "") extends Module(par, name) {
  //must have this at the beginning of the module declaration
  Module.currentModule = this
  
  //actual code goes here
  
  
  
  //must have these at the end of the module declaration
  Module.currentModule = parent
}

class DUT(par: Module = Module.currentModule, name: String = "") extends Module(par, name) {
  //must have this at the begining of the module declaration
  Module.currentModule = this
  //actual code goes here
  val g = Wire(name = "g", width = 32)
  val h = Wire(name = "h", width = 32)
  addInput(g)
  addOutput(h)
  val mem_cmd = DecoupledIO("mem_cmd", INPUT, 32)
  decoupledIOs += mem_cmd
  val a = Wire(name = "a",width = 3)
  val fsm = new FSM()
  val fsm2 = new FSM(name = "fsm")
  val b = Wire(name = "b")
  val c = Plus(a, mem_cmd.bits, "c")
  val d = Bool(name = "d") 
  val e = Wire(width = 5)
  val f = Plus(g, e, "f")
  Assign(f,h)
  val i = Not(e, "i")
  //must have these at the end of the module declaration 
  Module.currentModule = parent
}

object test {
  def main(args: Array[String]): Unit = {
    hdlMain(new DUT, "/home/eecs/wenyu/multithread-transform/simple-hdl-proj/generated/fsm.scala", "FSM")
  }
}
