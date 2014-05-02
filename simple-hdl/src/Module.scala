package hdl
import scala.collection.mutable._
import IODirection._

object Module {
  var currentModule:Module = null
}

abstract class Module(par: Module, name: String) {
  //connections to module hierarchy
  var isTop = false
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
  private var wireNameCounter = 0

  private var submoduleNameCounter = 0
  
  private var superOpNameCounter = 0

  //initializ parent and children pointers
  parent = par
  if(parent != null){
    par.children += this
  }
  instanceName = name

  //methods
  def addInput(wire:Wire) : Unit = {
    Predef.assert(wire.width != 0)
    Predef.assert(wire.name != "")
    inputs += wire
  }
  
  def addOutput(wire:Wire) : Unit = {
    Predef.assert(wire.width != 0)
    Predef.assert(wire.name != "")
    outputs += wire
  }
  
  def className: String = this.getClass.getName.split("""\.""")(1)
  
  def emitChiselSrc(outFile: java.io.FileWriter): Unit = {
    if(!codeGenerator.emittedModules.contains(name)){
      verify()
      findNonIONodes()
      //name nodes and submodules properly
      nameUnamedCircuitComponents()
      setEmissionNames()
      //gen code
      emitClassDeclaration(outFile)
      emitIOBundleDeclaration(outFile)
      emitWireDeclarations(outFile)
      emitSubmoduleDeclarations(outFile)
      emitWireConnections(outFile)
      emitCloser(outFile)
      //reset emissionNames so that verification will pass for submodules when they do their code gen
      resetEmissionNames()
      //mark this class as emitted so that it does not get code genned multiple times
      codeGenerator.emittedModules += name
    }
  }
  
  def verify(): Unit = {
    Predef.assert(instanceName != "")
    for(submodule <- children){
      Predef.assert(submodule.parent == this)
    }
    if(!isTop){
      Predef.assert(parent.children.contains(this))
    }
    for(node <- nodes){
      Predef.assert(node.module == this)
      node.verify()
    }
    for(superOp <- superOps){
      Predef.assert(superOp.module == this)
      superOp.verify()
    }
    
    //verify IOs
    for(input <- inputs){
      Predef.assert(input.inputs.length == 0)
    }
    for(output <- outputs){
      Predef.assert(output.consumers.length == 0)
    }
    for(decoupledIO <- decoupledIOs){
      if(decoupledIO.dir == INPUT){
        Predef.assert(decoupledIO.ready.consumers.length == 0)
        Predef.assert(decoupledIO.valid.inputs.length == 0)
        Predef.assert(decoupledIO.bits.inputs.length == 0)
      } else {
        Predef.assert(decoupledIO.ready.inputs.length == 0)
        Predef.assert(decoupledIO.valid.consumers.length == 0)
        Predef.assert(decoupledIO.bits.consumers.length == 0)
      }
    }
    for(varLatIO <- varLatIOs){
      Predef.assert(varLatIO.respPending.inputs.length == 0)
      Predef.assert(varLatIO.reqValid.consumers.length == 0)
      Predef.assert(varLatIO.reqBits.consumers.length == 0)
      Predef.assert(varLatIO.respBits.inputs.length == 0)
    }
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
      ioNodes += varLatIO.reqValid
      ioNodes += varLatIO.reqBits
      ioNodes += varLatIO.respBits
      ioNodes += varLatIO.respPending
    }
    for(node <- nodes){
      if(!ioNodes.contains(node)){
        nonIONodes += node
      }
    }
  }

  def nameUnamedCircuitComponents():Unit = {
    for(wire <- nodes.filter(_.isInstanceOf[Wire])){
      if(wire.name == ""){
        wire.name = "W" + wireNameCounter
        wireNameCounter = wireNameCounter + 1
      }
    }

    for(submodule <- children){
      if(submodule.instanceName == ""){
        submodule.instanceName = "M" + submoduleNameCounter
        submoduleNameCounter = submoduleNameCounter + 1
      }
    }
  
    for(superOp <- superOps){
      if(superOp.name == ""){
        superOp.name = "SO" + superOpNameCounter
        superOpNameCounter = superOpNameCounter + 1
      }
    }
  }
  
  def setEmissionNames():Unit = {
    //the emission names of the IO nodes need to be set differently depending if we are using them from the module they are declared in or if we are using them from the enclosing parent module
    for(input <- inputs){
      input.emissionName = "io." + input.name
    }
    for(output <- outputs){
      output.emissionName = "io." + output.name
    }
    for(decoupledIO <- decoupledIOs){
      decoupledIO.ready.emissionName = "io." + decoupledIO.name + "." + decoupledIO.ready.name
      decoupledIO.valid.emissionName = "io." + decoupledIO.name + "." + decoupledIO.valid.name
      decoupledIO.bits.emissionName = "io." + decoupledIO.name + "." + decoupledIO.bits.name
    }
    for(varLatIO <- varLatIOs){
      varLatIO.reqValid.emissionName = "io." + varLatIO.name + ".reqValid"
      varLatIO.reqBits.emissionName = "io." + varLatIO.name + ".reqBits"
    
      varLatIO.respBits.emissionName = "io." + varLatIO.name + ".respBits"
      varLatIO.respPending.emissionName = "io." + varLatIO.name + ".respPending"
    }
    for(submodule <- children){
    }
    
    //non IO nodes just have the same emission name as their original names
    for(node <- nonIONodes){
      node.emissionName = node.name
    }

    for(reg <- superOps.filter(_.isInstanceOf[Reg])){
      reg.emissionName = reg.name + "_reg"
    }

    for(mem <- superOps.filter(_.isInstanceOf[Mem])){
      mem.emissionName = mem.name
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
      outFile.write("    var " + varLatIO.name + " = new VarLatIO(" + varLatIO.reqBits.width + ", " + varLatIO.respBits.width + ")\n")
    }
    outFile.write("  }\n")
  }

  def emitWireDeclarations(outFile: java.io.FileWriter):Unit = {
    for(wire <- nonIONodes.filter(_.isInstanceOf[Wire])){
      var declaration = "  val " + wire.emissionName + " = "
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
    for(op <- nodes.filter(_.isInstanceOf[SimpleOp])){
      op match {
        case binary : BinaryOp => {
          outFile.write("  " + binary.consumers(0)._1.emissionName + " := " + binary.inputs(0).emissionName + " " + binary.chiselOperator + " " + binary.inputs(1).emissionName + "\n")
        }
        case unary: UnaryOp => {
          outFile.write("  " + unary.consumers(0)._1.emissionName + " := " + unary.chiselOperator + " " + unary.inputs(0).emissionName + "\n")
        }
        case assign : AssignOp => {
          outFile.write("  " + assign.consumers(0)._1.emissionName + " := " + assign.inputs(0).emissionName + "\n")
        }
        case mux : MuxOp => {
          outFile.write("  " + mux.consumers(0)._1.emissionName + " := Mux(" + mux.sel.emissionName + ", " + mux.in1.emissionName + ", " + mux.in0.emissionName + ")\n")
        }
        case extract : ExtractOp => {
          outFile.write("  " + extract.consumers(0)._1.emissionName + " := " + extract.inputs(0).emissionName + "(" + extract.highIndex + ", " + extract.lowIndex + ")\n")
        }
        case bitConst: BitConstOp => {
          Predef.assert(bitConst.width > 0)
          outFile.write("  " + bitConst.consumers(0)._1.emissionName + " := " + "Bits(" + bitConst.value + ", width = " + bitConst.width + ")\n")
        }
        case boolConst: BoolConstOp => {
          outFile.write("  " + boolConst.consumers(0)._1.emissionName + " := " + "Bool(" + boolConst.value + ")\n")
        }
        case cat: CatOp => {
          var output = "  " + cat.consumers(0)._1.emissionName + " := Cat(" + cat.inputs(0).emissionName
          for(i <- 1 until cat.inputs.length){
            output = output + ", " + cat.inputs(i).emissionName
          }
          output = output + ")\n"
          outFile.write(output)
        }
        case _ => {}
      }
    }
    for(superOp <- superOps){
      superOp match {
        case bitsReg : BitsReg => {
          //declare register
          var output = "  val " + bitsReg.emissionName + " = Reg(init = Bits(" + bitsReg.init 
          if(bitsReg.width > 0){
            output = output + ", width = " + bitsReg.width
          }
          output = output + "))\n"
          outFile.write(output)
          //emit register read port connection
          outFile.write("  " + bitsReg.readPort.consumers(0)._1.emissionName + " := " + bitsReg.emissionName + "\n")
          //emit register write port connections
          for(i <- 0 until bitsReg.writePorts.length){
            val writePort = bitsReg.writePorts(i)
            outFile.write("  when(" + writePort.en.emissionName + "){\n")
            outFile.write("    " + bitsReg.emissionName + " := " + writePort.data.emissionName + "\n")
            outFile.write("  }\n")
          }

        }
        case boolReg : BoolReg => {
          //declare register
          outFile.write("  val " + boolReg.emissionName + " = Reg(init = Bool(" + boolReg.init + "))\n")
          //emit register read port connection
          outFile.write("  " + boolReg.readPort.consumers(0)._1.emissionName + " := " + boolReg.emissionName + "\n")
          //emit register write port connections
          for(writePort <- boolReg.writePorts){
            outFile.write("  when(" + writePort.en.emissionName + "){\n")
            outFile.write("    " + boolReg.emissionName + " := " + writePort.data.emissionName + "\n")
            outFile.write("  }\n")
          }
        }
        case bitsMem : BitsMem => {
          //declare mem
          outFile.write("  val " + bitsMem.emissionName + " = Mem(Bits(width = " + bitsMem.width + "), " + bitsMem.depth + ")\n")
          //emit read ports
          Predef.assert(!bitsMem.isSeqRead)//implement sequential mem emission later
          for(readPort <- bitsMem.readPorts){
            outFile.write("  " + readPort.data.emissionName + " := " + bitsMem.emissionName + ".read(" + readPort.addr.emissionName + ")\n") 
          }
          //emit write ports
          for(writePort <- bitsMem.writePorts){
            outFile.write("  when(" + writePort.en.emissionName + "){\n")
            outFile.write("    " + bitsMem.emissionName + ".write(" + writePort.addr.emissionName + ", " + writePort.data.emissionName + ")\n")
            outFile.write("  }\n")
          }
        }
        case boolMem : BoolMem => {
          //declare mem
          outFile.write("  val " + boolMem.emissionName + " = Mem(Bool(), " + boolMem.depth + ")\n")
          //emit read ports
          Predef.assert(!boolMem.isSeqRead)//implement sequential mem emission later
          for(readPort <- boolMem.readPorts){
            outFile.write("  " + readPort.data.emissionName + " := " + boolMem.emissionName + ".read(" + readPort.addr.emissionName + ")\n") 
          }
          //emit write ports
          for(writePort <- boolMem.writePorts){
            outFile.write("  when(" + writePort.en.emissionName + "){\n")
            outFile.write("    " + boolMem.emissionName + ".write(" + writePort.addr.emissionName + ", " + writePort.data.emissionName + ")\n")
            outFile.write("  }\n")
          }

        }
        case _ => {}
      }
    }
  }

  def emitCloser(outFile: java.io.FileWriter):Unit = {
    outFile.write("}\n")
  }

  def resetEmissionNames():Unit = {
    for(node <- nodes){
      node.emissionName = ""
    }
    for(superOp <- superOps){
      superOp.emissionName = ""
    }
  }
}
