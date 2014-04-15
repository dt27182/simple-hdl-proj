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
  //io
  val in = Wire(name ="in", width = 32)
  val out = Wire(name = "out", width = 32)
  val mem_cmd = DecoupledIO("mem_cmd", OUTPUT, 32)
  val mem_resp = DecoupledIO("mem_resp", INPUT, 32)
  decoupledIOs += mem_cmd
  decoupledIOs += mem_resp
  addInput(in)
  addOutput(out)
  
  //body
  val accumulator = BitsReg(width = 32, name = "accumulator")
  val accumulator_in = Plus(accumulator, in, "accumulator_in")
  val accumulator_en = BoolConst(true, "accumulator_en")
  accumulator.getReg.addWrite(accumulator_en, accumulator_in)

  Assign(accumulator, out)
  
  val mem = BoolMem("mem", 4)
  val readEn = BoolConst(true, "readEn")
  val readData = Bool("readData")
  val readAddr = BitsConst(1, 2, "readAddr")
  mem.addRead(data = readData, addr = readAddr)
  val writeEn = BoolConst(true, "writeEn")
  val writeData = BoolConst(true, "writeData")
  val writeAddr = BitsConst(1, 2, "writeAddr")
  mem.addWrite(writeEn, writeData, writeAddr)

  val a = Wire("a")
  val b = Wire("b")
  val c = And(a, b, "c")
  val d = Or(a, b, "d")
  val e = Xor(a, b, "e")
  val f = Equal(a, b, "f")
  val g = NEqual(a, b, "g")
  val h = SL(a, b, "h")
  val i = SR(a, b, "i")
  val j = Mux(readData, a, b, "j")
  val k = Extract(a, 3, 3, "k")
  val l = Extract(a, 4, 3, "l")
  //must have these at the end of the module declaration 
  Module.currentModule = parent
}

object test {
  def main(args: Array[String]): Unit = {
    hdlMain(new FSM, "/home/eecs/wenyu/multithread-transform/simple-hdl-proj/generated/fsm.scala", "FSM")
  }
}
