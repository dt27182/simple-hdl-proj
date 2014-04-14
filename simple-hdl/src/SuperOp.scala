package hdl
import scala.collection.mutable._

//a SuperOP object contains many nodes; is used to represent registers and mems, which have read ports and write ports as individual nodes
abstract class SuperOp {
  var module:Module = null
  var name = ""
  var gennedName = ""
}

abstract class Reg extends SuperOp {
  var readPort:RegRead = null
  val writePorts:ArrayBuffer[RegWrite] = new ArrayBuffer[RegWrite]
}

class BitsReg extends Reg {
  var init:Int = 0
}

class BoolReg extends Reg {
  var init:Boolean = false
}

class RegRead extends Op {
}

class RegWrite extends Op {
}

abstract class Mem extends SuperOp {
  var isSeqRead = false
  val readPorts:ArrayBuffer[MemRead] = new ArrayBuffer[MemRead]
  val writePorts:ArrayBuffer[MemWrite] = new ArrayBuffer[MemWrite]
}

class MemRead extends Op {
}

class MemWrite extends Op {
}
