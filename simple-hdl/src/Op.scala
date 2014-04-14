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

object BitConst {
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
  
