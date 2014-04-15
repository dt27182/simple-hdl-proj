package hdl
import scala.collection.mutable._

abstract class Op extends Node {
  var numInputs = 0
  override def verify(): Unit = {
    Predef.assert(inputs.length == numInputs)
  }
}

abstract class SimpleOp extends Op {
}

abstract class MemberOp extends Op {
  var superOp: SuperOp = null
}

object ConstOp {
  def construct(name:String = "", width:Int, module: Module = Module.currentModule, constNode: ConstOp): Wire = {
    constNode.module = module
    module.nodes += constNode
    
    var output:Wire = null
    if(constNode.isInstanceOf[BoolConstOp]){
      output = new Bool(_name = name, _module = module)
    } else {
      output = new Wire(_name = name, _module = module)
    }
    output.inputs += constNode
    constNode.consumers += ((output, 0))
    return output
  }
}

abstract class ConstOp extends SimpleOp {
  numInputs = 0

  override def verify(): Unit = {
    super.verify
    Predef.assert(consumers.length == 1)
  }
}

object BitsConst {
  def apply(value:Int, width: Int, name:String = "", module:Module = Module.currentModule): Wire = {
    val bitConst = new BitConstOp(value, width)
    return ConstOp.construct(name, width, module, bitConst)
  }
}

class BitConstOp(_value: Int, _width: Int) extends ConstOp {
  val value = _value 
  width = _width
}

object BoolConst{
  def apply(value: Boolean, name:String = "",  module:Module = Module.currentModule): Wire = {
    val boolConst = new BoolConstOp(value)
    return ConstOp.construct(name, 1, module, boolConst)
  }
}

class BoolConstOp(_value: Boolean) extends ConstOp {
  val value = _value
}

object Assign {
  def apply(input: Wire, output: Wire, module: Module = Module.currentModule) = {
    Predef.assert(input.getClass == output.getClass)
    val assignOp = new AssignOp
    assignOp.inputs += input
    input.consumers += ((assignOp, 0))
    assignOp.module = module
    module.nodes += assignOp
    
    Predef.assert(output.inputs.length == 0)
    output.inputs += assignOp
    assignOp.consumers += ((output, 0))
  }
}

class AssignOp extends SimpleOp {
  numInputs = 1
  override def verify(): Unit = {
    super.verify
    Predef.assert(consumers.length == 1)
  }
}

object MuxCase {
  def apply(default: Wire, mapping: ArrayBuffer[(Wire, Wire)], name: String = "", module: Module = Module.currentModule): Wire = {
    var output = default
    for((en, data) <- mapping){
      Predef.assert(en.isInstanceOf[Bool])
      Predef.assert(default.getClass == data.getClass)
      output = Mux(en, data, output)
    }
    return output
  }
}

object Mux {
  def apply(sel: Wire, in1: Wire, in0: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    Predef.assert(sel.isInstanceOf[Bool])
    Predef.assert(in0.getClass == in1.getClass)
    
    val muxOp = new MuxOp
    muxOp.inputs += sel
    sel.consumers += ((muxOp, 0))
    muxOp.inputs += in1
    in1.consumers += ((muxOp, 1))
    muxOp.inputs += in0
    in0.consumers += ((muxOp, 2))

    var output:Wire = null
    if(in0.isInstanceOf[Bool]){
      output = new Bool(_name = name, _module = module)
    } else {
      output = new Wire(_name = name, _module = module)
    }

    output.inputs += muxOp
    muxOp.consumers += ((output, 0))

    muxOp.module = module
    module.nodes += muxOp
    return output
  }
}

class MuxOp extends SimpleOp {
  numInputs = 3
  override def verify(): Unit = {
    super.verify
    Predef.assert(consumers.length == 1)
  }
  def sel: Node = inputs(0)
  def in1: Node = inputs(1)
  def in0: Node = inputs(2)
}


object Extract {
  def apply(input: Wire, highIndex: Int, lowIndex:Int, name: String = "", module:Module = Module.currentModule): Wire = {
    Predef.assert(!input.isInstanceOf[Bool])
    Predef.assert(lowIndex >= 0)
    Predef.assert(highIndex >= lowIndex)
    val extractOp = new ExtractOp
    extractOp.lowIndex = lowIndex
    extractOp.highIndex = highIndex
    extractOp.inputs += input
    input.consumers += ((extractOp, 0))
    extractOp.module = module
    module.nodes += extractOp

    val output = new Wire(_name = name, _module = module)
    output.inputs += extractOp
    extractOp.consumers += ((output, 0))

    return output
  }

}

class ExtractOp extends SimpleOp {
  numInputs = 1
  var lowIndex = 0
  var highIndex = 0
  override def verify(): Unit = {
    super.verify
    Predef.assert(consumers.length == 1)
  }


}

object UnaryOp {
  def construct(input: Wire, name: String = "", module: Module = Module.currentModule, opNode: UnaryOp): Wire = {
    opNode.inputs += input
    input.consumers += ((opNode, 0))
    opNode.module = module
    module.nodes += opNode

    var output:Wire = null
    if(input.isInstanceOf[Bool]){
      output = new Bool(_name = name, _module = module)
    } else {
      output = new Wire(_name = name, _module = module)
    }
    output.inputs += opNode
    opNode.consumers += ((output, 0))
    return output
  }

}

abstract class UnaryOp extends SimpleOp {
  numInputs = 1
  var chiselOperator = ""

  override def verify(): Unit = {
    super.verify
    Predef.assert(consumers.length == 1)
  }
}

object Not {
  def apply(input: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    val notOp = new NotOp
    return UnaryOp.construct(input, name, module, notOp)
  }
}

class NotOp extends UnaryOp {
  chiselOperator = "~"
}

object BinaryOp {
  def construct(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule, opNode: BinaryOp): Wire = {
    Predef.assert(x.getClass == y.getClass)
    
    opNode.inputs += x
    x.consumers += ((opNode, 0))
    opNode.inputs += y
    y.consumers += ((opNode, 1))
    opNode.module = module
    module.nodes += opNode


    var output:Wire = null
    if(x.isInstanceOf[Bool]){
      output = new Bool(_name = name, _module = module)
    } else {
      output = new Wire(_name = name, _module = module)
    }
    output.inputs += opNode
    opNode.consumers += ((output, 0))
    return output
  }

}

abstract class BinaryOp extends SimpleOp {
  numInputs = 2
  var chiselOperator = ""
  
  override def verify(): Unit = {
    super.verify
    Predef.assert(chiselOperator != "")
    Predef.assert(consumers.length == 1)
  }
}

object Plus {
  def apply(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    val plusOp = new PlusOp
    return BinaryOp.construct(x, y, name, module, plusOp)
  }
}

class PlusOp extends BinaryOp {
  chiselOperator = "+"
}

object Minus {
  def apply(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    val op = new MinusOp
    return BinaryOp.construct(x, y, name, module, op)
  }
}

class MinusOp extends BinaryOp {
  chiselOperator = "-"
}

object And {
  def apply(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    val op = new AndOp
    return BinaryOp.construct(x, y, name, module, op)
  }
}

class AndOp extends BinaryOp {
  chiselOperator = "&"
}

object Or {
  def apply(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    val op = new OrOp
    return BinaryOp.construct(x, y, name, module, op)
  }
}

class OrOp extends BinaryOp {
  chiselOperator = "|"
}

object Xor {
  def apply(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    val op = new XorOp
    return BinaryOp.construct(x, y, name, module, op)
  }
}

class XorOp extends BinaryOp {
  chiselOperator = "^"
}

object Equal {
  def apply(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    val op = new EqualOp
    return BinaryOp.construct(x, y, name, module, op)
  }
}

class EqualOp extends BinaryOp {
  chiselOperator = "==="
}

object NEqual {
  def apply(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    val op = new NEqualOp
    return BinaryOp.construct(x, y, name, module, op)
  }
}

class NEqualOp extends BinaryOp {
  chiselOperator = "!="
}

object SL {
  def apply(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    Predef.assert(!y.isInstanceOf[Bool])
    val op = new SLOp
    return BinaryOp.construct(x, y, name, module, op)
  }
}

class SLOp extends BinaryOp {
  chiselOperator = "<<"
}

object SR {
  def apply(x: Wire, y: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    Predef.assert(!y.isInstanceOf[Bool])
    val op = new SROp
    return BinaryOp.construct(x, y, name, module, op)
  }
}

class SROp extends BinaryOp {
  chiselOperator = ">>"
}

