package hdl
import scala.collection.mutable._

//a SuperOP object contains many nodes; is used to represent registers and mems, which have read ports and write ports as individual nodes
abstract class SuperOp {
  var module:Module = null
  var name = ""
  //name used for code generation; should be "" until set by the code gen function
  var emissionName = ""
}

object Reg {
  def construct(name: String = "", module:Module = Module.currentModule, reg: Reg): Wire = {
    //create read port and output wire
    val readPort = new RegRead
    var outputWire: Wire = null
    if(reg.isInstanceOf[BoolReg]){
      outputWire = new Bool(_name = name, _module = module)
    } else {
      outputWire = new Wire(_name = name , _module = module)
    }
    readPort.consumers += ((outputWire, 0))
    outputWire.inputs += readPort
    
    reg.readPort = readPort
    readPort.superOp = reg
    
    readPort.module = module
    module.nodes += readPort
    
    reg.module = module
    module.superOps += reg
    
    return outputWire
  }
}

abstract class Reg extends SuperOp {
  var readPort:RegRead = null
  val writePorts:ArrayBuffer[RegWrite] = new ArrayBuffer[RegWrite]

  def addWrite(en: Wire, data: Wire): Unit = {
    Predef.assert(en.isInstanceOf[Bool])
    if(this.isInstanceOf[BoolReg]){
      Predef.assert(data.isInstanceOf[Bool])
    } else {
      Predef.assert(!data.isInstanceOf[Bool])
    }
    val writePort = new RegWrite
    writePort.inputs += en
    en.consumers += ((writePort, 0))
    writePort.inputs += data
    data.consumers += ((writePort, 1))
    
    writePorts += writePort
    writePort.superOp = this
    
    writePort.module = module
    module.nodes += writePort
  }

}

object BitsReg {
  def apply(init:Int = 0, width:Int, name: String = "", module: Module = Module.currentModule): Wire = {
    var bitsReg = new BitsReg
    bitsReg.init = init
    bitsReg.width = width
    bitsReg.name = name
    return Reg.construct(name, module, bitsReg)
  }
}

class BitsReg extends Reg {
  var init:Int = 0
  var width:Int = 0

  }

object BoolReg {
  def apply(init:Boolean = false, name: String = "", module: Module = Module.currentModule): Wire = {
    var boolReg = new BoolReg
    boolReg.init = init
    boolReg.name = name
    return Reg.construct(name, module, boolReg)
  }

}

class BoolReg extends Reg {
  var init:Boolean = false
}

class RegRead extends MemberOp {
}

class RegWrite extends MemberOp {
  numInputs = 2
  def en = inputs(0)
  def data = inputs(1)
}

abstract class Mem extends SuperOp {
  var isSeqRead = false
  val readPorts:ArrayBuffer[MemRead] = new ArrayBuffer[MemRead]
  val writePorts:ArrayBuffer[MemWrite] = new ArrayBuffer[MemWrite]
}

class MemRead extends MemberOp {
  numInputs = 2
  def addr = inputs(0)
  def en = inputs(1)
  def data = consumers(0)._1
}

class MemWrite extends MemberOp {
  numInputs = 3
  def addr = inputs(0)
  def en = inputs(1)
  def data = inputs(2)
}
