package hdl
import scala.collection.mutable._
import Direction._

object Module {
  var currentModule:Module = null
}

abstract class Module(par: Module, name: String) {
  //connections to module hierarchy
  var parent:Module = null
  val children = new ArrayBuffer[Module]
  //connections to internal nodes
  val nodes = new ArrayBuffer[Node]
  val superOps = new ArrayBuffer[SuperOp]
  val inputs = new ArrayBuffer[Node]
  val outputs = new ArrayBuffer[Node]
  val decoupledIOs = new ArrayBuffer[DecoupledIO]
  val varLatIOs = new ArrayBuffer[VarLatIO]
  var nonIONodes: ArrayBuffer[Node] = null
  //chisel code gen info
  var instanceName = ""
  //initializ parent and children pointers
  parent = par
  if(parent != null){
    par.children += this
  }
  instanceName = name

  //methods
  def addInput(wire:Wire) : Unit = {
    Predef.assert(wire.width != 0)
    inputs += wire
  }
  
  def addOutput(wire:Wire) : Unit = {
    Predef.assert(wire.width != 0)
    outputs += wire
  }

  def className: String = this.getClass.getName.split("""\.""")(1)
  
  def verify(): Unit = {
    instanceName != ""
  }

  def findNonIONodes(): Unit = {
    nonIONodes = new ArrayBuffer[Node]
    val ioNodes = new ArrayBuffer[Node]
    for(input <- inputs){
      ioNodes += input
    }
    for(output <- outputs){
      ioNodes += output
    }
    for(decoupledIO <- decoupledIOs){
      ioNodes += decoupledIO.ready
      ioNodes += decoupledIO.valid
      ioNodes += decoupledIO.bits
    }
    for(varLatIO <- varLatIOs){
      ioNodes += varLatIO.req.ready
      ioNodes += varLatIO.req.valid
      ioNodes += varLatIO.req.bits
      ioNodes += varLatIO.resp.ready
      ioNodes += varLatIO.resp.valid
      ioNodes += varLatIO.resp.bits
    }
    for(node <- nodes){
      if(!ioNodes.contains(node)){
        nonIONodes += node
      }
    }
  }

  def emitChiselSrc(outFile: java.io.FileWriter): Unit = {
    if(!codeGenerator.emittedModules.contains(name)){
      findNonIONodes()
      //name nodes and submodules properly
      nameUnamedNodes()
      nameUnamedSubmodules()
      setNodeGennedNames()
      //gen code
      emitClassDeclaration(outFile)
      emitIOBundleDeclaration(outFile)
      emitWireDeclarations(outFile)
      emitSubmoduleDeclarations(outFile)
      emitWireConnections(outFile)
      emitCloser(outFile)
      //mark this class as emitted so that it does not get code geneed multiple times
      codeGenerator.emittedModules += name
    }
  }

  def nameUnamedNodes():Unit = {
    var counter = 0
    for(wire <- nodes.filter(_.isInstanceOf[Wire])){
      if(wire.name == ""){
        wire.name = "W" + counter
        counter = counter + 1
      }
    }
  }

  def nameUnamedSubmodules():Unit = {
    var counter = 0
    for(submodule <- children){
      if(submodule.instanceName == ""){
        submodule.instanceName = "M" + counter
        counter = counter + 1
      }
    }
  }

  def setNodeGennedNames():Unit = {
    //the names of the IO nodes need to be set differently depending if we are using them from the module they are declared in or if we are using them from the enclosing parent module
    for(input <- inputs){
      input.gennedName = "io." + input.name
    }
    for(output <- outputs){
      output.gennedName = "io." + output.name
    }
    for(decoupledIO <- decoupledIOs){
      decoupledIO.ready.gennedName = "io." + decoupledIO.name + "." + decoupledIO.ready.name
      decoupledIO.valid.gennedName = "io." + decoupledIO.name + "." + decoupledIO.valid.name
      decoupledIO.bits.gennedName = "io." + decoupledIO.name + "." + decoupledIO.bits.name
    }
    for(varLatIO <- varLatIOs){
      
    }
    for(submodule <- children){
    }
    //none IO nodes just have the same name as their original names
    for(node <- nonIONodes){
      if(node.gennedName == ""){
        node.gennedName = node.name
      }
    }
  }

  def emitClassDeclaration(outFile: java.io.FileWriter):Unit = {
    outFile.write(" \n")
    outFile.write("class " + className + " extends Module {\n")
  }

  def emitIOBundleDeclaration(outFile: java.io.FileWriter):Unit = {
    outFile.write("  val io = new Bundle {\n")
    for(input <- inputs){
      var declaration = "    val " + input.name + " = "
      if(input.isInstanceOf[Bool]){
        declaration += "Bool(INPUT)"
      } else {
        declaration += "Bits(INPUT, width = " + input.width + ")"
      }
      declaration += "\n"
      outFile.write(declaration)
    }
    for(output <- outputs){
      var declaration = "    val " + output.name + " = "
      if(output.isInstanceOf[Bool]){
        declaration += "Bool(OUTPUT)"
      } else {
        declaration += "Bits(OUTPUT, width = " + output.width + ")"
      }
      declaration += "\n"
      outFile.write(declaration)
    }
    for(decoupledIO <- decoupledIOs){
      var declaration = "    var " + decoupledIO.name + " = new DecoupledIO(Bits(width = " + decoupledIO.bits.width + "))"
      if(decoupledIO.dir == INPUT){
        declaration += ".flip"
      }
      declaration += "\n"
      outFile.write(declaration)
      
    }
    for(varLatIO <- varLatIOs){
    }
    outFile.write("  }\n")
  }

  def emitWireDeclarations(outFile: java.io.FileWriter):Unit = {
    for(wire <- nonIONodes.filter(_.isInstanceOf[Wire])){
      var declaration = "  val " + wire.gennedName + " = "
      if(wire.isInstanceOf[Bool]){
        declaration = declaration + "Bool()"
      } else {
        declaration = declaration + "Bits("
        if(wire.width != 0){
          declaration = declaration + "width=" + wire.width
        }
        declaration = declaration + ")"
      }
      outFile.write(declaration + "\n" ) 
    }
  }

  def emitSubmoduleDeclarations(outFile: java.io.FileWriter):Unit = {
    for(submodule <- children){
      outFile.write("  val " + submodule.instanceName + " = Module(new " + submodule.className + ")\n")
    }
  }

  def emitWireConnections(outFile: java.io.FileWriter):Unit = {
    for(op <- nodes.filter(_.isInstanceOf[Op])){
      op match {
        case binary : BinaryOp => {
          outFile.write("  " + binary.consumers(0)._1.gennedName + " := " + binary.inputs(0).gennedName + " " + binary.chiselOperator + " " + binary.inputs(1).gennedName + "\n")
        }
        case unary: UnOp => {
          outFile.write("  " + unary.consumers(0)._1.gennedName + " := " + unary.chiselOperator + " " + unary.inputs(0).gennedName + "\n")
        }
        case assign : AssignOp => {
          outFile.write("  " + assign.consumers(0)._1.gennedName + " := " + assign.inputs(0).gennedName + "\n")
        }
        case bitConst: BitConstOp => {
          Predef.assert(bitConst.width > 0)
          outFile.write("  " + bitConst.consumers(0)._1.gennedName + " := " + "Bits(" + bitConst.value + ", width = " + bitConst.width + ")\n")
        }
        case boolConst: BoolConstOp => {
          outFile.write("  " + boolConst.consumers(0)._1.gennedName + " := " + "Bool(" + boolConst.value + ")\n")
        }
        case _ => {}
      }
    }
    for(SuperOp <- superOps){
    }
  }

  def emitCloser(outFile: java.io.FileWriter):Unit = {
    outFile.write("}\n")
  }
}
