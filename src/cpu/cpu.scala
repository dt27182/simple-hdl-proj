package FSM 
 
import hdl._ 
import scala.collection.mutable._
import IODirection._


class Cpu(par: Module = Module.currentModule, name: String = "") extends Module(par, name) {
  //must have this at the beginning of the module declaration
  Module.currentModule = this
  
  //actual code goes here
  //io
  val readData = DecoupledIO("readData", OUTPUT, 32)
  val readAddr = DecoupledIO("readAddr", INPUT, 4)
  decoupledIOs += readData
  decoupledIOs += readAddr
  
  val imemPort = VarLatIO("imemPort", 4, 32)
  val dmemPort = VarLatIO("dmemPort", 43, 32)
  varLatIOs += imemPort
  varLatIOs += dmemPort

  //body
  val pcReg = BitsReg(init = 0, width = 4, name = "pcReg") 
  val pcSpec = Plus(pcReg, BitsConst(1, 4), "pcSpec")
  val pcPlus4 = Plus(pcReg, BitsConst(1, 4), "pcPlus4")
  
  Assign(imemPort.reqValid, BoolConst(true))
  Assign(imemPort.reqBits, pcReg)

  val inst = Wire("inst")
  Assign(inst, imemPort.respBits)

  val rs1 = Extract(inst, 11, 8, "rs1")
  val rs2 = Extract(inst, 7, 4, "rs2")
  val rd = Extract(inst, 3, 0, "rd")
  val op = Extract(inst, 15, 12, "op")
  val imm = Extract(inst, 31, 16, "imm")
  val jmpTarget = Extract(imm, 3, 0, "jmpTarget")

  val isJmp = Equal(op, BitsConst(6, 4), "isJmp")
  val isNotJmp = NEqual(op, BitsConst(6, 4), "isNotJmp")
  
  pcReg.getReg.addWrite(isNotJmp, pcPlus4)
  pcReg.getReg.addWrite(isJmp, jmpTarget)
  
  val regfile = BitsMem(16, 32, name ="regfile")
  val rs1Data = Wire("rs1Data")
  val rs2Data = Wire("rs2Data")
  regfile.addRead(data = rs1Data, addr = rs1)
  regfile.addRead(data = rs2Data, addr = rs2)
  regfile.addRead(data = readData.bits, addr = readAddr.bits)

  val isExternalRead = Equal(op, BitsConst(7, 4), "isExternalRead")
  Assign(readAddr.ready, isExternalRead)
  Assign(readData.valid, isExternalRead)

  val operand1 = Mux(Equal(op, BitsConst(4, 4)), BitsConst(0, 32), rs1Data, "operand1")
  val operand2 = Mux(GT(op, BitsConst(1,4)), imm, rs2Data, "operand2")

  //alu
  val adderOut = Plus(operand1, operand2, "adderOut")
  val subtractOut = Minus(operand1, operand2, "subtractOut")
  
  val adderSel = Or(Or(Equal(op, BitsConst(0, 4)), Equal(op, BitsConst(2, 4))), Equal(op, BitsConst(4, 4)),"adderSel")
  val subtractSel = Or(Equal(op, BitsConst(1, 4)), Equal(op, BitsConst(3, 4)), "subtractSel")

  regfile.addWrite(adderSel, adderOut, rd)
  regfile.addWrite(subtractSel, subtractOut, rd)

  //mem
  val isLoad = Equal(op, BitsConst(8, 4), "isLoad")
  val isStore = Equal(op, BitsConst(9, 4), "isStore")

  //mem req setup
  val memWrite = Mux(isStore, BitsConst(1, 1), BitsConst(0, 1), "memWrite") 
  val reqBitWires = new ArrayBuffer[Wire]
  reqBitWires += memWrite
  reqBitWires += Extract(rs1Data, 9, 0)
  reqBitWires += rs2Data
  Assign(dmemPort.reqBits, Cat(reqBitWires))
  Assign(dmemPort.reqValid, Or(isLoad, isStore))
  //mem resp setup
  regfile.addWrite(isLoad, dmemPort.respBits, rd)

  //must have these at the end of the module declaration
  Module.currentModule = parent

  //auto pipelining specification
  autoMultiThread.setNumThreads(4)
  autoMultiThread.setStageNum(4)  
  autoMultiThread.setRegWriteStage(pcReg.getReg, 1)
  autoMultiThread.setStage(pcSpec, 0)
}



object test {
  def main(args: Array[String]): Unit = {
    hdlMain(new Cpu(name="top"), "/home/eecs/wenyu/multithread-transform/simple-hdl-proj/generated/cpu-dut.scala", "Cpu")
  }
}
