package hdl
import scala.collection.mutable._

object autoPipeline {
  var PipelineComponentNodes: ArrayBuffer[Node] = null
  var PipelineComponentSuperOps: ArrayBuffer[SuperOp] = null 
  var ArchitecturalRegs: ArrayBuffer[Reg] = null
  var ArchitecturalMems: ArrayBuffer[Mem] = null
  var VarLatIOs: ArrayBuffer[VarLatIO] = null
  
  var autoNodeGraph: ArrayBuffer[AutoNode] = null
  var autoSourceNodes: ArrayBuffer[AutoNode] = null
  var autoSinkNodes: ArrayBuffer[AutoNode] = null
  
  def apply[T <: Module] (top: T) : Unit = {
    gatherSpecialNodes(top)
    createAutoNodeGraph(top)
  }
  
  def gatherSpecialNodes[T <: Module] (top: T) : Unit = {
    println("gathering special nodes")
    PipelineComponentNodes = new ArrayBuffer[Node]
    PipelineComponentSuperOps = new ArrayBuffer[SuperOp]
    ArchitecturalRegs = new ArrayBuffer[Reg]
    ArchitecturalMems = new ArrayBuffer[Mem]
    VarLatIOs = new ArrayBuffer[VarLatIO]
    
    def registerNodes(module: Module): Unit = {
      for(node <- module.nodes){
        PipelineComponentNodes += node
      }
      for(childModule <- module.children){
        registerNodes(childModule)
      }
    }
    registerNodes(top)
    
    def registerSuperOps(module: Module): Unit = {
      for(superOp <- module.superOps){
        PipelineComponentSuperOps += superOp
      }
      for(childModule <- module.children){
        registerSuperOps(childModule)
      }
    }
    registerSuperOps(top)

    //mark architectural registers
    for(superOp <- PipelineComponentSuperOps){
      if (superOp.isInstanceOf[Reg]) {
        ArchitecturalRegs += superOp.asInstanceOf[Reg]
      }
    }
    
    //mark architecural mems
    for(superOp <- PipelineComponentSuperOps){
      if (superOp.isInstanceOf[Mem]) {
        ArchitecturalMems += superOp.asInstanceOf[Mem]
      }
    }

    //mark variable latency modules
    for(varLatIO <- top.varLatIOs){
      VarLatIOs += varLatIO
    }
    
    
    /*//specify read/write point and IO stage numbers if auto annotate is on
    if(autoAnnotate){
      for(archReg <- ArchitecturalRegs){
        setRegReadStage(archReg, 0)
        setRegWriteStage(archReg, autoAnnotateStageNum - 1)
      }
      for(tMem <-ArchitecturalMems){
        setTmemWriteStage(tMem, autoAnnotateStageNum - 1)
      }
      for(io <- ioNodes){
        //setDecoupledIOStage(io, autoAnnotateStageNum/2)
        setDecoupledIOStageAuto(io, autoAnnotateStageNum/2)
      }
    }*/
  }

  def createAutoNodeGraph[T <: Module] (top: T) : Unit = {
    val nodeToAutoNodeMap = new HashMap[Node, AutoNode]
    val autoNodeGraph = new ArrayBuffer[AutoNode]
    autoSourceNodes = new ArrayBuffer[AutoNode]
    autoSinkNodes = new ArrayBuffer[AutoNode]

    /*//create AutoLogic nodes for all chisel nodes that require a stage
    for(reg <- ArchitecturalRegs){
      val readPoint = new AutoLogic
      readPoint.name = reg.name
      readPoint.isSource = true
      readPoint.findStage(reg, userAnnotatedStages)
      readPoint.outputChiselNodes += reg
      nodeToAutoNodeMap(reg) = readPoint
      autoNodeGraph += readPoint
      autoSourceNodes += readPoint

      val writePoint = new AutoLogic
      writePoint.name = reg.name + "_writepoint"
      writePoint.isSink = true
      for(producer <- reg.getProducers){
        writePoint.findStage(producer, userAnnotatedStages)
        writePoint.inputChiselNodes += producer
        nodeToAutoNodeMap(producer) = writePoint
      }
      autoNodeGraph += writePoint
      autoSinkNodes += writePoint
    }

    for(tMem <- ArchitecturalMems){
      val readPoint = new AutoLogic
      readPoint.name = tMem.name + "_readports"
      readPoint.delay = 5.0
      if(tMem.isSeqRead){
        readPoint.isSeqReadPort = true
      }
      for(i <- 0 until tMem.readPortNum){
        val readAddr = tMem.io.reads(i).adr
        val readData = tMem.io.reads(i).dat
        val readEn = tMem.io.reads(i).is
        readPoint.findStage(readAddr, userAnnotatedStages)
        readPoint.findStage(readEn, userAnnotatedStages)
        readPoint.inputChiselNodes += readAddr
        readPoint.inputChiselNodes += readEn
        readPoint.outputChiselNodes += readData
        nodeToAutoNodeMap(readAddr) = readPoint
        nodeToAutoNodeMap(readEn) = readPoint
        nodeToAutoNodeMap(readData) = readPoint
      }
      autoNodeGraph += readPoint
      
      val writePoint = new AutoLogic
      writePoint.name = tMem.name + "_writeports"
      writePoint.isSink = true
      for(i <- 0 until tMem.virtWritePortNum){
        val writeAddr = tMem.io.writes(i).adr
        val writeData = tMem.io.writes(i).dat
        val writeEn = tMem.io.writes(i).is
        writePoint.findStage(writeAddr, userAnnotatedStages)
        writePoint.findStage(writeData, userAnnotatedStages)
        writePoint.findStage(writeEn, userAnnotatedStages)
        writePoint.inputChiselNodes += writeAddr
        writePoint.inputChiselNodes += writeData
        writePoint.inputChiselNodes += writeEn
        nodeToAutoNodeMap(writeAddr) = writePoint
        nodeToAutoNodeMap(writeData) = writePoint
        nodeToAutoNodeMap(writeEn) = writePoint
      }
      autoNodeGraph += writePoint
      autoSinkNodes += writePoint
    }

    for(varLatUnit <- VarLatIOs){
      val inputs = varLatUnit.io.flatten.map(_._2).filter(_.dir == INPUT)
      val outputs = varLatUnit.io.flatten.map(_._2).filter(_.dir == OUTPUT)
      val varLatUnitNode = new AutoLogic
      varLatUnitNode.name = varLatUnit.name
      varLatUnitNode.delay = 5.0
      for(input <- inputs){
        varLatUnitNode.findStage(input, userAnnotatedStages)
        varLatUnitNode.inputChiselNodes += input
        nodeToAutoNodeMap(input) = varLatUnitNode
      }
      for(output <- outputs){
        varLatUnitNode.findStage(output, userAnnotatedStages)
        varLatUnitNode.outputChiselNodes += output
        nodeToAutoNodeMap(output) = varLatUnitNode
      }
      autoNodeGraph += varLatUnitNode 
    }
    
    for(io <- ioNodes){
      val ioNode = new AutoLogic
      ioNode.name = "io_node"
      ioNode.isDecoupledIO = true
      val dataBits = io.bits.flatten.map(_._2)
      
      ioNode.findStage(io.ready, userAnnotatedStages)
      ioNode.findStageAuto(io.ready, autoAnnotatedStages)
      if(io.ready.dir == INPUT){
        ioNode.outputChiselNodes += io.ready
      } else {
        ioNode.inputChiselNodes += io.ready
      }
      nodeToAutoNodeMap(io.ready) = ioNode

      ioNode.findStage(io.valid, userAnnotatedStages)
      ioNode.findStageAuto(io.valid, autoAnnotatedStages)

      if(io.valid.dir == INPUT){
        ioNode.outputChiselNodes += io.valid
      } else {
        ioNode.inputChiselNodes += io.valid
      }
      nodeToAutoNodeMap(io.valid) = ioNode

      for(data <- dataBits){
        ioNode.findStage(data, userAnnotatedStages)
        ioNode.findStageAuto(data, autoAnnotatedStages)
        if(data.dir == INPUT){
          ioNode.outputChiselNodes += data
        } else {
          ioNode.inputChiselNodes += data
        }
        nodeToAutoNodeMap(data) = ioNode
      }
      autoNodeGraph += ioNode
    }
    

    for(node <- requireStageSet){
      if(!isSource(node) && !isSink(node)){
        val autoNode = new AutoLogic
        autoNode.name = node.name
        autoNode.delay = node.delay
        autoNode.findStage(node, userAnnotatedStages)
        autoNode.inputChiselNodes += node
        autoNode.outputChiselNodes += node
        nodeToAutoNodeMap(node) = autoNode
        autoNodeGraph += autoNode
      }
    }

    //connect autologic nodes
    for(autoNode <- autoNodeGraph){
      for(input <- autoNode.asInstanceOf[AutoLogic].inputChiselNodes){
        for(i <- 0 until input.inputs.length){
          if(requireStage(input.inputs(i))){
            val autoInput = nodeToAutoNodeMap(input.inputs(i))
            autoNode.inputs += autoInput
            autoNode.asInstanceOf[AutoLogic].inputMap(autoInput) = ((input, i))
          }
        }
      }
      if(autoNode.inputs.isEmpty){
        autoNode.isSource = true
        autoSourceNodes += autoNode
      }
      for(output <- autoNode.asInstanceOf[AutoLogic].outputChiselNodes){
        for(consumer <- APConsumers(output).map(_._1).filter(requireStage(_))){
          autoNode.consumers += nodeToAutoNodeMap(consumer)
        }
      }
      if(autoNode.consumers.isEmpty){
        autoNode.isSink = true
        autoSinkNodes += autoNode
      }
    }

    //insert AutoWires between the AutoLogicNodes
    val bfsQueue = new ScalaQueue[AutoLogic]
    val visited = new HashSet[AutoLogic]
    for(autoNode <- autoSinkNodes){
      bfsQueue.enqueue(autoNode.asInstanceOf[AutoLogic])
    }
    while(!bfsQueue.isEmpty){
      val currentNode = bfsQueue.dequeue
      if(!visited.contains(currentNode)){
        visited += currentNode
        for(input <- currentNode.inputs){
          if(!visited.contains(input.asInstanceOf[AutoLogic]) && !autoSourceNodes.contains(input)){
            bfsQueue.enqueue(input.asInstanceOf[AutoLogic])
          }
        }
        for(i <- 0 until currentNode.inputs.length){
          val input = currentNode.inputs(i)
          //insert AutoWire
          val autoWire = new AutoWire
          autoWire.inputs += input
          autoWire.consumers += currentNode
          autoWire.consumerChiselNode = currentNode.inputMap(input)._1
          autoWire.consumerChiselNodeInputNum = currentNode.inputMap(input)._2
          autoNodeGraph += autoWire
          //fix input pointers on currentNode
          currentNode.inputs(i) = autoWire

          //fix consumers pointers on input
          var foundConsumer = false
          for(j <- 0 until input.consumers.length){
            if(input.consumers(j) == currentNode && !foundConsumer){
              foundConsumer = true
              input.consumers(j) = autoWire
            }
          }
        }
      }
    }

    return autoNodeGraph*/
  }
}
