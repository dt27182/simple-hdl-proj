package FSM 
 
import hdl._ 
import scala.collection.mutable._
import IODirection._

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
  
  val mem = BoolMem(4, name="mem")
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

class TransactionMemDUT(par: Module = Module.currentModule, name: String = "") extends Module(par, name) {
  //must have this at the begining of the module declaration
  Module.currentModule = this
  //actual code goes here
  //io
  val addr_in = DecoupledIO("addr_in", INPUT, 2)
  val data_out = DecoupledIO("data_out", OUTPUT, 32)
  decoupledIOs += addr_in
  decoupledIOs += data_out
  
  val icache = VarLatIO("icache", 32, 32)
  varLatIOs += icache
  //body
  val mem = BitsMem(4, 32, name = "mem")
   
  val write_data = Wire("write_data")
  val write_addr = Wire("write_addr", width = 2)
  val write_en = Bool("write_en")
  
  mem.addWrite(write_en, write_data, write_addr)

  val memReadData = Wire("memReadData")
  mem.addRead(data = memReadData, addr = addr_in.bits)
  
  val currentState = BitsReg(width = 4, name = "currentState")
  val readData = BitsReg(width = 32, name = "readData")

  val write0 = BitsConst(0, 4, "write0")
  val write1 = BitsConst(1, 4, "write1")
  val write2 = BitsConst(2, 4, "write2")
  val write3 = BitsConst(3, 4, "write3")
  val waitForRead = BitsConst(4, 4, "waitForRead")
  val sendReadData = BitsConst(5, 4, "sendReadData")
  
  val addr_in_ready_mapping = new ArrayBuffer[(Wire, Wire)]
  val data_out_bits_mapping = new ArrayBuffer[(Wire, Wire)]
  val data_out_valid_mapping = new ArrayBuffer[(Wire, Wire)]
  val write_data_mapping = new ArrayBuffer[(Wire, Wire)]
  val write_addr_mapping = new ArrayBuffer[(Wire, Wire)]
  val write_en_mapping = new ArrayBuffer[(Wire, Wire)]
  
  val stateEqWrite0 = Equal(currentState, write0, "stateEqWrite0")
  val stateEqWrite1 = Equal(currentState, write1, "stateEqWrite1")
  val stateEqWrite2 = Equal(currentState, write2, "stateEqWrite2")
  val stateEqWrite3 = Equal(currentState, write3, "stateEqWrite3")
  val stateEqWaitForRead = Equal(currentState, waitForRead, "stateEqWaitForRead")
  val stateEqSendReadData = Equal(currentState, sendReadData, "stateEqSendReadData")

  addr_in_ready_mapping += ((stateEqWaitForRead, BoolConst(true)))
  Assign(MuxCase(BoolConst(false), addr_in_ready_mapping),addr_in.ready)

  data_out_bits_mapping += ((stateEqSendReadData, readData))
  Assign(MuxCase(BitsConst(0, width = 32), data_out_bits_mapping), data_out.bits)

  data_out_valid_mapping += ((stateEqSendReadData, BoolConst(true)))
  Assign(MuxCase(BoolConst(false), data_out_valid_mapping), data_out.valid)

  write_data_mapping += ((stateEqWrite0, BitsConst(3, width = 32)))
  write_data_mapping += ((stateEqWrite1, BitsConst(2, width = 32)))
  write_data_mapping += ((stateEqWrite2, BitsConst(1, width = 32)))
  write_data_mapping += ((stateEqWrite3, BitsConst(0, width = 32)))

  Assign(MuxCase(BitsConst(0, width = 32), write_data_mapping), write_data)

  write_addr_mapping += ((stateEqWrite0, BitsConst(0, width = 2)))
  write_addr_mapping += ((stateEqWrite1, BitsConst(1, width = 2)))
  write_addr_mapping += ((stateEqWrite2, BitsConst(2, width = 2)))
  write_addr_mapping += ((stateEqWrite3, BitsConst(3, width = 2)))

  Assign(MuxCase(BitsConst(0, width = 2), write_addr_mapping), write_addr)

  write_en_mapping += ((stateEqWrite0, BoolConst(true)))
  write_en_mapping += ((stateEqWrite1, BoolConst(true)))
  write_en_mapping += ((stateEqWrite2, BoolConst(true)))
  write_en_mapping += ((stateEqWrite3, BoolConst(true)))

  Assign(MuxCase(BoolConst(false), write_en_mapping), write_en)

  currentState.getReg.addWrite(stateEqWrite0, write1)
  currentState.getReg.addWrite(stateEqWrite1, write2)
  currentState.getReg.addWrite(stateEqWrite2, write3)
  currentState.getReg.addWrite(stateEqWrite3, waitForRead)
  currentState.getReg.addWrite(And(stateEqWaitForRead, addr_in.valid), sendReadData)
  currentState.getReg.addWrite(And(stateEqSendReadData, data_out.ready), waitForRead)

  readData.getReg.addWrite(And(stateEqWaitForRead, addr_in.valid), memReadData)
  
  /*
  val accumulator = BitsReg(width = 32, name = "accumulator")
  val accumulator_in = Plus(accumulator, in, "accumulator_in")
  val accumulator_en = BoolConst(true, "accumulator_en")
  accumulator.getReg.addWrite(accumulator_en, accumulator_in)

  Assign(accumulator, out)
  
  val mem = BoolMem(4, name="mem")
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
  val l = Extract(a, 4, 3, "l")*/
  //must have these at the end of the module declaration 
  Module.currentModule = parent

}

object test {
  def main(args: Array[String]): Unit = {
    hdlMain(new TransactionMemDUT(name="top"), "/home/eecs/wenyu/multithread-transform/simple-hdl-proj/generated/fsm.scala", "FSM")
  }
}
