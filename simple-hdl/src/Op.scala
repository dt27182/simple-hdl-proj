package hdl
import scala.collection.mutable._

abstract class Op extends Node {
  var numInputs = 0
  override def verify(): Unit = {
    Predef.assert(inputs.length == numInputs)
  }
}

abstract class ConstOp extends Op {
  numInputs = 0

  override def verify(): Unit = {
    super.verify
    Predef.assert(consumers.length == 1)
  }
}

abstract class UnOp extends Op {
  numInputs = 1
  var chiselOperator = ""

  override def verify(): Unit = {
    super.verify
    Predef.assert(consumers.length == 1)
  }
}

object Assign {
  def apply(input: Wire, output: Wire, module: Module = Module.currentModule) = {
    val assignOp = new AssignOp
    assignOp.inputs += input
    input.consumers += ((assignOp, 0))
    assignOp.module = module
    module.nodes += assignOp
    
    output.inputs += assignOp
    assignOp.consumers += ((output, 0))
  }
}

class AssignOp extends UnOp {
}

object Not {
  def apply(input: Wire, name: String = "", module: Module = Module.currentModule): Wire = {
    val notOp = new NotOp
    notOp.inputs += input
    input.consumers += ((notOp, 0))
    notOp.module = module
    module.nodes += notOp

    val output = new Wire(_name = name, _module = module)
    output.inputs += notOp
    notOp.consumers += ((output, 0))
    return output
  }
}

class NotOp extends UnOp {
  chiselOperator = "~"
}

abstract class BinaryOp extends Op{
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
    plusOp.inputs += x
    x.consumers += ((plusOp, 0))
    plusOp.inputs += y
    y.consumers += ((plusOp, 1))
    plusOp.module = module
    module.nodes += plusOp
    val output = new Wire(_name = name, _module = module)
    output.inputs += plusOp
    plusOp.consumers += ((output, 0))
    return output
  }
}

class PlusOp extends BinaryOp {
  chiselOperator = "+"
}
