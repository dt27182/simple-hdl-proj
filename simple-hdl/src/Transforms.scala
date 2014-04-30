package hdl
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Queue=>ScalaQueue}
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.BitSet
import IODirection._
import Direction._

object autoPipeline {
  //front end specification variables
  var autoAnnotate = false
  var autoAnnotateStageNum = 0
  var userAnnotatedStages = new HashMap[Node, Int]()
  
  //node orgranization variables
  var top: Module = null
  var ArchitecturalRegs: ArrayBuffer[Reg] = null
  var ArchitecturalMems: ArrayBuffer[Mem] = null
  var ioNodes: ArrayBuffer[DecoupledIO] = null
  var VarLatIOs: ArrayBuffer[VarLatIO] = null
 
  val regWritePorts = new HashSet[Node] 
  val regReadPorts = new HashSet[Node] 
  val memWritePorts = new HashSet[Node] 
  val memReadPorts = new HashSet[Node] 
  val inputNodes = new HashSet[Node] //list of all auto pipelined module Input Nodes
  val outputNodes = new HashSet[Node] //list of all auto pipelined module Output Nodes
 
  var requireStageSet = new HashSet[Node]
   
  //autoNode variables 
  var autoNodeGraph: ArrayBuffer[AutoNode] = null
  var autoSourceNodes: ArrayBuffer[AutoNode] = null
  var autoSinkNodes: ArrayBuffer[AutoNode] = null
  
  
  //logic generation variables
  var nodeToStageMap: HashMap[Node, Int] = null
  
  val regRAWHazards = new HashMap[(RegRead, RegWrite, Int), Bool] //map of (reg read, reg write, write stage) -> RAW signal
  val memRAWHazards = new HashMap[(MemRead, MemWrite, Int), Bool] //map of (mem read, mem write, write stage) -> RAW signal 
  
  val decoupledIOBusySignals = new HashMap[(DecoupledIO, Int), Bool]//map of (DecoupledIO, DecoupledIO stage) -> IO busy signal
  val varLatIOBusySignals = new HashMap[(VarLatIO, DecoupledIO, Int), Bool]//map of (VarLatIO, resp/req DecoupledIO, VarLatIO stage) -> IO busy signal

  var pipelineLength = 0
  var pipelineRegs = new HashMap[Int, ArrayBuffer[Reg]]()
  var globalStall: Bool = null  

  val stageValids = new ArrayBuffer[Bool]//the bool at index i is the valid signal of stage i
  val prevStageValidRegs = new ArrayBuffer[Bool]//the bool at index i is the registered version of the valid signal of stage i
  val stageStalls = new ArrayBuffer[Bool]//the bool at index i is the stall signal of stage i aka the signal that determines if the contents of stage i should move forward to stage i + 1
  val stageKills = new ArrayBuffer[Bool]//the bool at index i is the kill signal of stage i
  val stageNoRAWSignals = new ArrayBuffer[Bool]
  val stageNoIOBusySignals = new ArrayBuffer[Bool]

  //helper methods
  def isSource(node: Node) =  {
    regReadPorts.contains(node) || inputNodes.contains(node)
  }

  def isSink(node: Node) = {
    regWritePorts.contains(node) || memWritePorts.contains(node) || outputNodes.contains(node)
  }

  def sourceNodes(): HashSet[Node] = {
    regReadPorts | inputNodes
  }
  def sinkNodes(): HashSet[Node] = {
    regWritePorts | memWritePorts | outputNodes 
  }
  def requireStage(node: Node): Boolean = {
    return requireStageSet.contains(node)
  }
  
  def getStage(n: Node): Int = {
    if (nodeToStageMap.contains(n))
      return nodeToStageMap(n)
    else if(!requireStage(n))
      return -1
    else
      Predef.assert(false, "node does not have astage number: " + n)
      return -2
  }
  
  //checks if n is a user defined pipeline register
  def isPipeLineReg(reg: Reg): Boolean = {
    var result = false
    for(regArray <- pipelineRegs.values){
      if(regArray.contains(reg)){
        result = true
      }
    }
    result
  }
    
  //frontend specification methods
  def setStageNum(num: Int) = {
    autoAnnotate = true
    autoAnnotateStageNum = num
  }

  def setStage(node: Node, stage: Int) = {
    Predef.assert(!userAnnotatedStages.contains(node))
    userAnnotatedStages(node) = stage
  }

  def setRegReadStage(reg: Reg, stage: Int) = {
    setStage(reg.readPort, stage)
  }

  def setRegWriteStage(reg: Reg, stage: Int) = {
    for(writePort <- reg.writePorts){
      setStage(writePort, stage)
    }
  }

  def setMemReadStage(mem: Mem, stage: Int) = {
    for(readPort <- mem.readPorts){
      setStage(readPort, stage)
    }
  }
  
  def setMemWriteStage(mem: Mem, stage: Int) = {
    for(writePort <- mem.writePorts){
      setStage(writePort, stage)
    }
  }
  
  def setInputDecoupledIOStage(decoupledIO: DecoupledIO, stage: Int) = {
    Predef.assert(decoupledIO.dir == INPUT)
    setStage(decoupledIO.ready, stage)
    setStage(decoupledIO.bits, stage)
  }

  def setOutputDecoupledIOStage(decoupledIO: DecoupledIO, stage: Int) = {
    Predef.assert(decoupledIO.dir == OUTPUT)
    setStage(decoupledIO.valid, stage)
    setStage(decoupledIO.bits, stage)
  }
 
  def setVarLatIOStage(varLatIO: VarLatIO, stage: Int) = {
    setInputDecoupledIOStage(varLatIO.resp, stage)
    setOutputDecoupledIOStage(varLatIO.req, stage)
    setStage(varLatIO.respPending, stage)
  }
  
  //transform methods
  def apply[T <: Module] (_top: T) : Unit = {
    //parse in pipelining specification
    top = _top
    verifyInitialNodeGraph()
    gatherSpecialNodes()
    autoAnnotateStages()
    setPipelineLength(userAnnotatedStages.map(_._2).max + 1)
    
    //find pipeline register placement and insert pipeline registers
    findNodesRequireStage()
    createAutoNodeGraph()
    propagateStages()
    verifyLegalStageColoring()
    optimizeRegisterPlacement()
    verifyLegalStageColoring()
    insertPipelineRegisters()

    autoNodeGraph = null
    autoSourceNodes = null
    autoSinkNodes = null
    
    //find pipeline hazards and generate hazard resolution logic
    generateRAWHazardSignals()
    generateIOBusySignals()
    generateStageNoRAWSignals()
    generateStageNoIOBusySignals()
    generateStageValidSignals()
    generateStageStallSignals()
    connectStageValidsToConsumers()
    connectStageStallsToConsumers()
    
    //fix up node graph to pass verify in code gen
    insertAssignOpsBetweenWires()

    //verify that our generated node graph is legal
    top.verify()
  }

  def verifyInitialNodeGraph() : Unit = {
    Predef.assert(top.inputs.length == 0)
    Predef.assert(top.outputs.length == 0)
    for(decoupledIO <- top.decoupledIOs){
      if(decoupledIO.dir == INPUT){
        Predef.assert(decoupledIO.valid.consumers.length == 0)
        Predef.assert(decoupledIO.bits.consumers.length > 0)
        Predef.assert(decoupledIO.ready.inputs.length > 0)
      } else {
        Predef.assert(decoupledIO.valid.inputs.length > 0)
        Predef.assert(decoupledIO.bits.inputs.length > 0)
        Predef.assert(decoupledIO.ready.consumers.length == 0)
      }
    }
  }

  def gatherSpecialNodes () : Unit = {
    println("gathering special nodes")
    ArchitecturalRegs = new ArrayBuffer[Reg]
    ArchitecturalMems = new ArrayBuffer[Mem]
    ioNodes = new ArrayBuffer[DecoupledIO]
    VarLatIOs = new ArrayBuffer[VarLatIO]
    
    //mark architectural registers
    for(superOp <- top.superOps){
      if (superOp.isInstanceOf[Reg]) {
        ArchitecturalRegs += superOp.asInstanceOf[Reg]
      }
    }
    
    //mark architecural mems
    for(superOp <- top.superOps){
      if (superOp.isInstanceOf[Mem]) {
        ArchitecturalMems += superOp.asInstanceOf[Mem]
      }
    }
    
    //mark DecoupledIOs
    for(decoupledIO <- top.decoupledIOs){
      ioNodes += decoupledIO
    }

    //mark variable latency modules
    for(varLatIO <- top.varLatIOs){
      VarLatIOs += varLatIO
    }

    for(reg <- ArchitecturalRegs){
      regReadPorts += reg.readPort
      for(writePort <- reg.writePorts){
        regWritePorts += writePort
      }
    }

    for(mem <- ArchitecturalMems){
      for(readPort <- mem.readPorts){
        memReadPorts += readPort
      }
      for(writePort <- mem.writePorts){
        memWritePorts += writePort
      }
    }

    for(decoupledIO <- ioNodes){
      if(decoupledIO.dir == INPUT){
        inputNodes += decoupledIO.bits
        outputNodes += decoupledIO.ready
      } else {
        outputNodes += decoupledIO.valid
        outputNodes += decoupledIO.bits
      }
    }

    for(varLatIO <- VarLatIOs){
      inputNodes += varLatIO.resp.bits
      outputNodes += varLatIO.resp.ready
      outputNodes += varLatIO.req.valid
      outputNodes += varLatIO.req.bits
    }
  }

  def autoAnnotateStages() : Unit = {
    //specify read/write point and IO stage numbers if auto annotate is on
    if(autoAnnotate){
      for(archReg <- ArchitecturalRegs){
        if(!userAnnotatedStages.contains(archReg.readPort)){
          setRegReadStage(archReg, 0)
        }
        if(!userAnnotatedStages.contains(archReg.writePorts(0))){
          setRegWriteStage(archReg, autoAnnotateStageNum - 1)
        }
      }
      for(mem <-ArchitecturalMems){
        if(!userAnnotatedStages.contains(mem.writePorts(0))){
          setMemWriteStage(mem, autoAnnotateStageNum - 1)
        }
      }
      for(decoupledIO <- ioNodes){
        if(decoupledIO.dir == OUTPUT){
          if(!userAnnotatedStages.contains(decoupledIO.bits)){
            setOutputDecoupledIOStage(decoupledIO, autoAnnotateStageNum - 1)
          }
        }
      }
    }
  }

  def setPipelineLength (length: Int) = {
    pipelineLength = length
    for (i <- 0 until pipelineLength - 1) {
      pipelineRegs += (i -> new ArrayBuffer[Reg]())
    }
    
    for (i <- 0 until pipelineLength) {
      val valid = Bool("PipeStage_Valid_" + i, top)
      stageValids += valid
    }

    prevStageValidRegs += BoolConst(true, module = top).asInstanceOf[Bool]
    for (i <- 1 until pipelineLength) {
      val validReg = BoolReg(false, "Stage_" + i + "_valid_reg", top)
      validReg.getReg.addWrite(BoolConst(true, module = top), stageValids(i - 1))
      prevStageValidRegs += validReg.asInstanceOf[Bool]
    }
    
    for (i <- 0 until pipelineLength) {
      val stall = Bool("PipeStage_Stall_" + i, top)
      stageStalls += stall
    }
    
    for (i <- 0 until pipelineLength) {
      val kill = Bool("PipeStage_Kill_" + i, top)
      stageKills += kill
    }
    
    for (i <- 0 until pipelineLength) {
      val noRAW = Bool("PipeStage_NoRAW_" + i, top)
      stageNoRAWSignals += noRAW
    }
    
    for (i <- 0 until pipelineLength) {
      val noIOBusy = Bool("PipeStage_NoIOBusy_" + i, top)
      stageNoIOBusySignals += noIOBusy
    }
  }
  def findNodesRequireStage() : Unit = {
    //mark nodes reachable from sourceNodes through the consumer pointer as requiring a stage number
    val bfsQueue = new ScalaQueue[Node]
    val visited = new HashSet[Node]
    for(node <- sourceNodes()){
      bfsQueue.enqueue(node)
    }
    while(!bfsQueue.isEmpty){
      val currentNode = bfsQueue.dequeue
      if(!isSink(currentNode)){
        for(child <- currentNode.consumers.map(_._1)){
          if(!visited.contains(child) && !isSink(child)){
            bfsQueue.enqueue(child)
          }
        }
        requireStageSet += currentNode
      }
      visited +=currentNode
    }

    //mark all source and sink nodes as requiring a stage number(need to do this separately because not all of them are reachable by following consumers pointers
    for(node <- sourceNodes()){
      requireStageSet += node
    }
    for(node <- sinkNodes()){
      requireStageSet += node
    }
  }

  def createAutoNodeGraph() : Unit = {
    val nodeToAutoNodeMap = new HashMap[Node, AutoNode]
    autoNodeGraph = new ArrayBuffer[AutoNode]
    autoSourceNodes = new ArrayBuffer[AutoNode]
    autoSinkNodes = new ArrayBuffer[AutoNode]

    //create AutoLogic nodes for all chisel nodes that require a stage
    for(reg <- ArchitecturalRegs){
      val readPoint = new AutoLogic
      readPoint.name = reg.name
      readPoint.isSource = true
      readPoint.findStage(reg.readPort, userAnnotatedStages)
      readPoint.outputChiselNodes += reg.readPort
      nodeToAutoNodeMap(reg.readPort) = readPoint
      autoNodeGraph += readPoint
      autoSourceNodes += readPoint

      val writePoint = new AutoLogic
      writePoint.name = reg.name + "_writepoint"
      writePoint.isSink = true
      for(writePort <- reg.writePorts){
        writePoint.findStage(writePort, userAnnotatedStages)
        writePoint.inputChiselNodes += writePort
        nodeToAutoNodeMap(writePort) = writePoint
      }
      autoNodeGraph += writePoint
      autoSinkNodes += writePoint
    }

    for(mem <- ArchitecturalMems){
      val readPoint = new AutoLogic
      readPoint.name = mem.name + "_readports"
      readPoint.delay = 5.0
      if(mem.isSeqRead){
        readPoint.isSeqReadPort = true
      }
      for(readPort <- mem.readPorts){
        readPoint.findStage(readPort, userAnnotatedStages)
        readPoint.inputChiselNodes += readPort
        readPoint.outputChiselNodes += readPort
        nodeToAutoNodeMap(readPort) = readPoint
      }
      autoNodeGraph += readPoint
      
      val writePoint = new AutoLogic
      writePoint.name = mem.name + "_writeports"
      writePoint.isSink = true
      for(writePort <- mem.writePorts){
        writePoint.findStage(writePort, userAnnotatedStages)
        writePoint.inputChiselNodes += writePort
        writePoint.outputChiselNodes += writePort
        nodeToAutoNodeMap(writePort) = writePoint
      }
      autoNodeGraph += writePoint
      autoSinkNodes += writePoint
    }

    for(varLatIO <- VarLatIOs){
      val varLatIONode = new AutoLogic
      varLatIONode.name = varLatIO.name
      varLatIONode.delay = 5.0
      
      varLatIONode.findStage(varLatIO.req.valid, userAnnotatedStages)
      varLatIONode.inputChiselNodes += varLatIO.req.valid
      nodeToAutoNodeMap(varLatIO.req.valid) = varLatIONode
      
      varLatIONode.findStage(varLatIO.req.bits, userAnnotatedStages)
      varLatIONode.inputChiselNodes += varLatIO.req.bits
      nodeToAutoNodeMap(varLatIO.req.bits) = varLatIONode
      
      varLatIONode.findStage(varLatIO.resp.ready, userAnnotatedStages)
      varLatIONode.inputChiselNodes += varLatIO.resp.ready
      nodeToAutoNodeMap(varLatIO.resp.ready) = varLatIONode

      varLatIONode.findStage(varLatIO.resp.bits, userAnnotatedStages)
      varLatIONode.outputChiselNodes += varLatIO.resp.bits
      nodeToAutoNodeMap(varLatIO.resp.bits) = varLatIONode

      autoNodeGraph += varLatIONode 
    }
    
    for(decoupledIO <- ioNodes){
      val ioNode = new AutoLogic
      ioNode.name = decoupledIO.name
      ioNode.isDecoupledIO = true

      if(decoupledIO.dir == INPUT){
        ioNode.findStage(decoupledIO.ready, userAnnotatedStages)
        ioNode.inputChiselNodes += decoupledIO.ready
        nodeToAutoNodeMap(decoupledIO.ready) = ioNode

        ioNode.findStage(decoupledIO.bits, userAnnotatedStages)
        ioNode.outputChiselNodes += decoupledIO.bits
        nodeToAutoNodeMap(decoupledIO.bits) = ioNode
      } else {
        ioNode.findStage(decoupledIO.valid, userAnnotatedStages)
        ioNode.inputChiselNodes += decoupledIO.valid
        nodeToAutoNodeMap(decoupledIO.valid) = ioNode

        ioNode.findStage(decoupledIO.bits, userAnnotatedStages)
        ioNode.inputChiselNodes += decoupledIO.bits
        nodeToAutoNodeMap(decoupledIO.bits) = ioNode
        
        ioNode.isSink = true
        autoSinkNodes += ioNode
      }
      autoNodeGraph += ioNode
    }
    
    
    for(node <- requireStageSet){
      if(!nodeToAutoNodeMap.contains(node)){
        val autoNode = new AutoLogic
        autoNode.name = node.name
        if(node.isInstanceOf[Op]){
          autoNode.delay = 5.0
        } else {
          autoNode.delay = 0.0
        }
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
        for(consumer <- output.consumers.map(_._1).filter(requireStage(_))){
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
  }
  
  def propagateStages() : Unit = {
    val bfsQueue = new ScalaQueue[(AutoNode, Direction)] //the 2nd item of the tuple indicateds which direction the node should propagate its stage number to. True means propagate to outputs, false means propagate to inputs
    val retryNodes = new ArrayBuffer[(AutoNode, Direction)] //list of nodes that need to retry their stage propagation because their children weren't ready
  
    //direction == FORWARD means parent is input of child, direction == BACKWARD means parent is consumer of child
    def propagatedToChild(parent:AutoNode, child: AutoNode, direction: Direction) : Boolean = {
      var result = false
      if(direction == FORWARD){
        result = parent.outputStage == child.inputStage
      } else {
        result = parent.inputStage == child.outputStage
      }
      return result
    }
    
    def propagatedToAllChildren(node: AutoNode, direction: Direction) : Boolean = {
      var children = node.inputs
      if(direction == FORWARD){
        children = node.consumers
      }
      var result = true
      for(child <- children){
        result = result && propagatedToChild(node, child, direction)
      }
      return result
    }
    
    def propagateToChildren(node: AutoNode, direction: Direction) = {
      val propagatedChildren = new ArrayBuffer[AutoNode] 
      
      //determine if we need/can propagate to child and return the stage number that should be propagated to child; direction = false means child is producer of node, direction = true means child is consumer of node
      def childEligibleForPropagation(child: AutoNode, direction: Direction): Boolean = {
        val childWasPropagated = propagatedToChild(node, child, direction)
        val wireToLogic = node.isInstanceOf[AutoWire] && child.isInstanceOf[AutoLogic] && child.propagatedTo
        var allParentsResolved = true
        var childParents = new ArrayBuffer[AutoNode]
        if(direction == FORWARD){
          for(childProducer <- child.inputs){
            childParents += childProducer
          }
        } else {
          for(childConsumer <- child.consumers){
            childParents += childConsumer
          }
        }
        var edgeParentStage:Int = 0//this is the minimum stage of child's consumers when direction == true, this is the maximum stage of child's producers when direction == false
        if(direction == FORWARD){
          edgeParentStage = 0
        } else {
          edgeParentStage = Int.MaxValue
        }

        for(parent <- childParents){
          if(!parent.propagatedTo){
            allParentsResolved = false
          } else {
            if(direction == FORWARD){
              if(parent.outputStage > edgeParentStage){
                edgeParentStage = parent.outputStage
              }
            } else {
              if(parent.inputStage < edgeParentStage){
                edgeParentStage = parent.inputStage
              }
            }
          }
        }
        val childEligible = !childWasPropagated && !wireToLogic && allParentsResolved
        return childEligible
      }
      //propagate node's stage number to child and record all new nodes that have been propagated to; direction = false means child is producer of node, direction = true means child is consumer of node
      def doPropagation(child: AutoNode, direction: Direction) = {
        var childParents = new ArrayBuffer[AutoNode]
        if(direction == FORWARD){
          for(childProducer <- child.inputs){
            childParents += childProducer
          }
        } else {
          for(childConsumer <- child.consumers){
            childParents += childConsumer
          }
        }
        
        var edgeParentStage:Int = 0//this is the minimum stage of child's consumers when direction == true, this is the maximum stage of child's producers when direction == false
        if(direction == FORWARD){
          edgeParentStage = 0
        } else {
          edgeParentStage = Int.MaxValue
        }

        for(parent <- childParents){
          if(direction == FORWARD){
            if(parent.outputStage > edgeParentStage){
              edgeParentStage = parent.outputStage
            }
          } else {
            if(parent.inputStage < edgeParentStage){
              edgeParentStage = parent.inputStage
            }
          }
        }
        //propagate stage to child
        child.propagateStage(edgeParentStage, direction)
        
        //propagate child stage back to its parents
        for(parent <- childParents){
          if(direction == FORWARD){
            parent.outputStage = edgeParentStage
          } else {
            parent.inputStage = edgeParentStage
          }
        }
        
        //set propagatedChildren
        propagatedChildren += child
      }

      var children:Seq[AutoNode] = null
      if(direction == FORWARD){//propagate to consumers
        children = node.consumers
      } else {//propagate to producers
        children = node.inputs
      }
      
      for(child <- children) {
        //check if we need/can propagate to child
        if(childEligibleForPropagation(child, direction)){
          //propagate stage to child
          doPropagation(child, direction)
        }
      }
      propagatedChildren
    }
    

    //initialize bfs queue and coloredNodes with user annotated nodes
    for(autoNode <- autoNodeGraph){
      if(autoNode.isUserAnnotated){
        if(!autoNode.isSink){
          retryNodes += ((autoNode, FORWARD))
        }
        if(!autoNode.isSource){
          retryNodes += ((autoNode, BACKWARD))
        }
      }
    }
    
    //do stage propagation
    while(!retryNodes.isEmpty){
      for((node, direction) <- retryNodes){
        bfsQueue.enqueue(((node, direction)))
      }
      retryNodes.clear
      while(!bfsQueue.isEmpty){
        val current = bfsQueue.dequeue
        val currentNode = current._1
        val currentDirection = current._2
        if(currentNode.inputStage == currentNode.outputStage || currentNode.isSeqReadPort){
          var childrenPropagatedTo:ArrayBuffer[AutoNode] = null
          childrenPropagatedTo = propagateToChildren(currentNode, currentDirection)
          for(child <- childrenPropagatedTo){
            if((child.inputStage == child.outputStage || child.isSeqReadPort) && !child.isSource && !child.isSink && !child.isUserAnnotated){
              bfsQueue.enqueue(((child, currentDirection)))
            }
          }
          if(!propagatedToAllChildren(currentNode, currentDirection)){
            retryNodes += ((currentNode, currentDirection))
          }
        }
      }
    }
  }

  
  def optimizeRegisterPlacement(): Unit = { 
    val nodeArrivalTimes = new HashMap[AutoNode, Double]
    val forwardArrivalTimes = new HashMap[AutoNode, Double]
    val backwardArrivalTimes = new HashMap[AutoNode, Double]

    val stageDelays = ArrayBuffer.fill(pipelineLength)(0.0)
    
    def calculateArrivalTimes() = {
      def findArrivalTime(node: AutoNode, dir: Direction): Double = {
        var arrivalTimes = forwardArrivalTimes
        if(dir == FORWARD){
          arrivalTimes = forwardArrivalTimes
        } else {
          arrivalTimes = backwardArrivalTimes
        }

        if(arrivalTimes.contains(node)){
          return arrivalTimes(node)
        } else if(node.inputStage != node.outputStage) {
          arrivalTimes(node) = 0.0
          return 0.0
        } else if(node.isDecoupledIO){
          arrivalTimes(node) = 0.0
          return 0.0
        } else {
          var arrivalTime :Double= 0.0
          val parents = new ArrayBuffer[AutoNode]
          if(dir == FORWARD){
            for(input <- node.inputs){
              parents += input
            }
          } else {
            for(consumer <- node.consumers){
              parents += consumer
            }
          }
          for(parent <- parents){
            arrivalTime = Math.max(arrivalTime, findArrivalTime(parent, dir))
          }
          arrivalTimes(node) = arrivalTime + node.delay
          return arrivalTimes(node)
        }
      }
      //find forward delay times
      forwardArrivalTimes.clear()
      for(node <- autoNodeGraph){
        if(node.inputStage != node.outputStage){
          for(input <- node.inputs){
            findArrivalTime(input, FORWARD)
          }
        }
      }
      for(node <- autoSinkNodes){
        findArrivalTime(node, FORWARD)
      }
      //find backward delay times
      backwardArrivalTimes.clear()
      for(node <- autoNodeGraph){
        if(node.inputStage != node.outputStage){
          for(consumer <- node.consumers){
            findArrivalTime(consumer, BACKWARD)
          }
        }
      }
      for(node <- autoSourceNodes){
        findArrivalTime(node, BACKWARD)
      }
    }
    
    def findUnpipelinedPathLength(): Double = {
      val nodeArrivalTimes = new HashMap[AutoNode, Double]
      def calculateArrivalTimes() = {
        def findArrivalTime(node: AutoNode) : Double = {
          if(nodeArrivalTimes.contains(node)){
            return nodeArrivalTimes(node)
          } else if(node.isDecoupledIO){
            nodeArrivalTimes(node) = 0.0
            return 0.0
          } else {
            var arrivalTime: Double= 0.0
            val parents = new ArrayBuffer[AutoNode]
            for(input <- node.inputs){
              parents += input
            }

            for(parent <- parents){
              arrivalTime = Math.max(arrivalTime, findArrivalTime(parent))
            }
            
            nodeArrivalTimes(node) = arrivalTime + node.delay
            return nodeArrivalTimes(node)
          }
        }
        nodeArrivalTimes.clear()
        for(node <- autoSinkNodes){
          findArrivalTime(node)
        }
      }
    
      calculateArrivalTimes()
      var maxLength = 0.0
      for(node <- nodeArrivalTimes.keys){
        maxLength = Math.max(maxLength, nodeArrivalTimes(node))
      }
      return maxLength
    }
    
    def findStageDelays() = {
      for(i <- 0 until pipelineLength){
        stageDelays(i) = 0.0
      }
      for(node <- autoNodeGraph){
        if(node.inputStage == node.outputStage){
          if(forwardArrivalTimes.contains(node)){
            if(forwardArrivalTimes(node) > stageDelays(node.inputStage)){
              stageDelays(node.inputStage) = forwardArrivalTimes(node)
            }
          }
        }
      }
    }

    def movePipelineBoundary(boundaryNum: Int, direction: Direction) = {
      val boundaryNodes = new ArrayBuffer[AutoNode]
      val possibleMoveNodes = new ArrayBuffer[AutoNode]
      val eligibleMoveNodes = new ArrayBuffer[AutoNode]
      
      //find nodes from possibleMoveNodes that can have the pipeline boundary be legally moved in "direction" accross the node and store them in eligibleMoveNodes
      def findEligibleNodes(direction: Direction) = {
        for(node <- possibleMoveNodes){
          if(!node.isUserAnnotated && node.isInstanceOf[AutoLogic] && !node.isSource && !node.isSink){
            var parentsEligible = true
            val parents = new ArrayBuffer[AutoNode]
            
            if(direction == FORWARD){
              for(input <- node.inputs){
                parents += input
              }
            } else {
              for(consumer <- node.consumers){
                parents += consumer
              }
            }
          
            for(parent <- parents){
              Predef.assert(parent.inputStage >= 0 && parent.outputStage >= 0)
              if(parent.inputStage == parent.outputStage){
                parentsEligible = false
              } else {
                if(direction == FORWARD){
                  Predef.assert(parent.outputStage == node.inputStage)
                } else {
                  Predef.assert(parent.inputStage == node.outputStage)
                }
              }
            }
            if(parentsEligible){
              eligibleMoveNodes += node
            }
          }
        }
      }
      //move pipeline boundary in "direction" across "node"
      def moveNode(movedNode: AutoNode, direction: Direction) = {
        val nodeStage = movedNode.inputStage
        var stageDelta = 0
        if(direction == FORWARD){
          stageDelta = -1
        } else {
          stageDelta = 1
        }
        
        movedNode.inputStage = movedNode.inputStage + stageDelta
        movedNode.outputStage = movedNode.outputStage + stageDelta

        val oldBoundaryWires = new ArrayBuffer[AutoWire]
        val newBoundaryWires = new ArrayBuffer[AutoWire]

        for(input <- movedNode.inputs){
          if(direction == FORWARD){
            oldBoundaryWires += input.asInstanceOf[AutoWire]
          } else {
            newBoundaryWires += input.asInstanceOf[AutoWire]
          }
        }
        for(consumer <- movedNode.consumers){
          if(direction == FORWARD){
            newBoundaryWires += consumer.asInstanceOf[AutoWire]
          } else {
            oldBoundaryWires += consumer.asInstanceOf[AutoWire]
          }
        }
       
        for(wire <- oldBoundaryWires){
          if(direction == FORWARD){
            wire.outputStage = movedNode.inputStage
          } else {
            wire.inputStage = movedNode.outputStage
          }
        }
        for(wire <- newBoundaryWires){
          if(direction == FORWARD){
            wire.inputStage = movedNode.outputStage
          } else {
            wire.outputStage = movedNode.inputStage
          }
        }
      }
      
      //populate boundaryNodes
	    for(node <- autoNodeGraph.filter(n => n.inputStage != n.outputStage && !n.isSeqReadPort)){
        if(node.inputStage == boundaryNum && node.outputStage == boundaryNum + 1){
          boundaryNodes += node
        } else {
          if(direction == FORWARD){
            if(node.outputStage == boundaryNum + 1 && node.inputStage < boundaryNum + 1){
              boundaryNodes += node
            }
          } else if(direction == BACKWARD) {
            if(node.inputStage == boundaryNum && node.outputStage > boundaryNum){
              boundaryNodes += node
            }
          }
        }
      }
      //find nodes that are producers/consumers of the boundary nodes
      if(direction == FORWARD){
        for(node <- boundaryNodes){
          for(consumer <- node.consumers){
            possibleMoveNodes += consumer
          }
        }
      } else if(direction == BACKWARD){
        for(node <- boundaryNodes){
          for(input <- node.inputs){
            possibleMoveNodes += input
          }
        }
      }

      //find nodes that are eligible to be moved accross the pipeline boundary
      findEligibleNodes(direction)
      //find eligible node with the highest dealy
      var criticalNode: AutoNode = null
      var highestDelay = 0.0
      var arrivalTimes = forwardArrivalTimes
      if(direction == FORWARD){
        arrivalTimes = backwardArrivalTimes
      } else {
        arrivalTimes = forwardArrivalTimes
      }
      for(node <- eligibleMoveNodes){
        val nodeDelay = arrivalTimes(node)
        if(nodeDelay >= highestDelay){
          criticalNode = node
          highestDelay = nodeDelay
        }
      }
      if(eligibleMoveNodes.length > 0){
        moveNode(criticalNode, direction)
      } 
    }

    var iterCount = 1
    calculateArrivalTimes()
    findStageDelays()
    println(stageDelays)
    val unPipelinedDelay = findUnpipelinedPathLength()
    var oldMaxDelay = unPipelinedDelay
    println("max unpipelined path: " + unPipelinedDelay)
    println("max delay before optimizeation: " + stageDelays.max)
    //while(!(iterCount % 100 == 0 && oldMaxDelay == stageDelays.max) && iterCount < 10000){
    while(stageDelays.max > unPipelinedDelay/pipelineLength && iterCount < 5000){   
      //find pipeline stage with longest delay
      val criticalStageNum = stageDelays.indexOf(stageDelays.max)
      //determine which pipeline boundary to move
      for(pipeBoundaryNum <- 0 until pipelineLength - 1){
        calculateArrivalTimes()
        findStageDelays()
        if(stageDelays(pipeBoundaryNum) < stageDelays(pipeBoundaryNum + 1)){
          movePipelineBoundary(pipeBoundaryNum, FORWARD)
        } else if(stageDelays(pipeBoundaryNum) > stageDelays(pipeBoundaryNum + 1)){
          movePipelineBoundary(pipeBoundaryNum, BACKWARD)
        }
      }
      iterCount = iterCount + 1
      if(iterCount % 100 == 0){
        oldMaxDelay = stageDelays.max
      }
    }
    calculateArrivalTimes()
    findStageDelays()
    println(stageDelays)
    println("max delay after optimizeation: " + stageDelays.max)
    println("iteration count: " + iterCount)
    visualizeAutoLogicGraph(autoNodeGraph, "stages.gv")
    visualizeAutoLogicGraph(forwardArrivalTimes, "fdelays.gv")
    visualizeAutoLogicGraph(backwardArrivalTimes, "bdelays.gv")
  }
  
  def verifyLegalStageColoring(): Unit  = {
    //add check that no node is both a register read point and a register write point
    
    //check that all nodes have a stage number
    for(node <- autoNodeGraph){
      Predef.assert(node.propagatedTo, "progate stages failed to give all nodes a stage number")
      Predef.assert(node.inputStage >= 0)
      Predef.assert(node.outputStage >= 0)
      Predef.assert(node.outputStage >= node.inputStage)
    }

    //check that all combinational logic nodes have all inputs from the same stage
    for(node <- autoNodeGraph.filter(_.isInstanceOf[AutoLogic])){
      if(node.isSeqReadPort){
        Predef.assert(node.outputStage == node.inputStage + 1)
      } else {
        Predef.assert(node.outputStage == node.inputStage)
      }
      for(input <- node.inputs){
        Predef.assert(input.outputStage == node.inputStage, "combinational logic node does not have inputs all in the same stage")
      }
    }
    
    //check that all architectural register has write points in the same stage and that its write stage >= its read stage
    
    //check all tmems read and write ports are annotated; data, addr, and enable of each port is in same stage; all read ports are in the same stage; all write ports are in same stage; write ports have stage >= read port stage
    
  }
  
  def visualizeAutoLogicGraph(autoNodes: ArrayBuffer[AutoNode], fileName: String) = {
    val outFile = new java.io.FileWriter("/home/eecs/wenyu/multithread-transform/" + fileName)
    outFile.write("digraph G {\n")
    outFile.write("graph [rankdir=LR];\n")
    var nameEnum = 0
    val nodeNames = new HashMap[AutoNode, String]
    for(node <- autoNodes){
      var fillColor = "red"
      if(node.isInstanceOf[AutoLogic]){
        fillColor = "green"
      }
      outFile.write("n" + nameEnum + " [label=\"" + node.name + " " + """\n""" + node.inputStage + " " + node.outputStage + " " + node.propagatedTo + "\"" + ", style = filled, fillcolor = " + fillColor + "];\n")
      nodeNames(node) = "n" + nameEnum
      nameEnum = nameEnum + 1
    }
    for(node <- autoNodes){
      if(!node.isSource){
        for(input <- node.inputs){
          if(nodeNames.contains(input)){
            outFile.write(nodeNames(input) + " -> " + nodeNames(node) + ";\n")
          }
        }
      }
    }
    outFile.write("}\n")
    outFile.close
  }
  
  def visualizeAutoLogicGraph(autoNodeMap: HashMap[AutoNode, _], fileName: String) = {
    val outFile = new java.io.FileWriter("/home/eecs/wenyu/multithread-transform/" + fileName)
    outFile.write("digraph G {\n")
    outFile.write("graph [rankdir=LR];\n")
    var nameEnum = 0
    val nodeNames = new HashMap[AutoNode, String]
    for(node <- autoNodeMap.keys){
      var fillColor = "red"
      if(node.isInstanceOf[AutoLogic]){
        fillColor = "green"
      }
      outFile.write("n" + nameEnum + " [label=\"" + node.name + " " + """\n""" + node.stages + " " + autoNodeMap(node) + "\"" + ", style = filled, fillcolor = " + fillColor + "];\n")
      nodeNames(node) = "n" + nameEnum
      nameEnum = nameEnum + 1
    }
    for(node <- autoNodeMap.keys){
      if(!node.isSource){
        for(input <- node.inputs){
          if(nodeNames.contains(input)){
            outFile.write(nodeNames(input) + " -> " + nodeNames(node) + ";\n")
          }
        }
      }
    }
    outFile.write("}\n")
    outFile.close
  }

  def insertPipelineRegisters() : Unit = {
    var nameCounter = 0
    nodeToStageMap = new HashMap[Node, Int]
    
    for(wire <- autoNodeGraph.filter(_.isInstanceOf[AutoWire]).map(_.asInstanceOf[AutoWire])){
      if(wire.inputStage != wire.outputStage){
        val stageDifference = wire.outputStage - wire.inputStage
        Predef.assert(stageDifference > 0, stageDifference)
        val originalName = wire.consumerChiselNode.name
        var currentChiselNode = insertWireOnInput(wire.consumerChiselNode, wire.consumerChiselNodeInputNum)
        nodeToStageMap(currentChiselNode) = Math.min(wire.inputStage, wire.outputStage)
        for(i <- 0 until stageDifference){
          currentChiselNode = insertRegister(currentChiselNode, "Stage_" + (Math.min(wire.inputStage,wire.outputStage) + i) + "_" + "PipeReg_"+ nameCounter + originalName)
          nodeToStageMap(currentChiselNode) = wire.inputStage + i + 1
          nodeToStageMap(currentChiselNode.getReg.readPort) = wire.inputStage + i + 1
          Predef.assert(currentChiselNode.getReg.writePorts.length == 1)
          nodeToStageMap(currentChiselNode.getReg.writePorts(0)) = wire.inputStage + i
          nameCounter = nameCounter + 1
          pipelineRegs(Math.min(wire.inputStage, wire.outputStage) + i) += currentChiselNode.getReg
        }
      }
    }
    
    for(node <- autoNodeGraph.filter(_.isInstanceOf[AutoLogic])){
      for(inputChiselNode <- node.asInstanceOf[AutoLogic].inputChiselNodes){
        nodeToStageMap(inputChiselNode) = node.inputStage
      }
      for(outputChiselNode <- node.asInstanceOf[AutoLogic].outputChiselNodes){
        nodeToStageMap(outputChiselNode) = node.outputStage
      }
    }
  }

  //insert additional bit node between *output* and its input specified by *inputNum*
  def insertWireOnInput[T <: Node](output: T, inputNum: Int): Wire = {
    var newWire:Wire = null
    val input = output.inputs(inputNum)
    if(output.isInstanceOf[Op]){
      if(input.isInstanceOf[Bool]){
        newWire = Bool(module = input.module)
      } else {
        newWire = Wire(module = input.module)
      }
    } else {
      if(output.isInstanceOf[Bool]){
        newWire = Bool(module = output.module)
      } else {
        newWire = Wire(module = output.module)
      }
    }
    
    for(i <- 0 until input.consumers.size){
      val inputConsumer = input.consumers(i)._1
      val inputConsumerInputIndex = input.consumers(i)._2
      if(inputConsumer == output && inputConsumerInputIndex == inputNum){
        input.consumers(i) = ((newWire, 0))
        output.inputs(inputNum) = newWire
        newWire.inputs += input
        newWire.consumers += ((output,inputConsumerInputIndex))
      }
    }
    return newWire
  }

  //inserts register between *input* and its consumers
  def insertRegister[T <: Wire] (input: T, name: String) : Wire = {
    var newRegOutput: Wire = null
    if(input.isInstanceOf[Bool]){
      newRegOutput = BoolReg(false, name, input.module)
    } else {
      newRegOutput = BitsReg(1, name = name, module = input.module)
    }
    
    for(i <- 0 until input.consumers.size){
      val consumer = input.consumers(i)._1
      val consumerInputIndex = input.consumers(i)._2
      consumer.inputs(consumerInputIndex) = newRegOutput
      newRegOutput.consumers += ((consumer, consumerInputIndex))
    }
    input.consumers.clear()
    newRegOutput.getReg.addWrite(BoolConst(true, module = input.module), input)
    return newRegOutput
  }
  
  def generateRAWHazardSignals() = {
    println("searching for hazards...")
    
    // handshaking stalls from variable latency components; stall entire pipe when data is not readyfor now
    /*globalStall = Bool(false)
    for (variableLatencyComponent <- VariableLatencyComponents) {
      val stall = variableLatencyComponent.io.req.valid && (!variableLatencyComponent.req_ready || !variableLatencyComponent.resp_valid)
      globalStall = globalStall || stall
    }*/

    // raw stalls
    var raw_counter = 0
    for (reg <- ArchitecturalRegs) {
      val readStage = getStage(reg.readPort)
      for (writePort <- reg.writePorts){
        val writeEn = writePort.en
        val writeData = writePort.data
        val writeStage = Math.max(getStage(writeEn), getStage(writeData))
        Predef.assert(writeStage > -1, "both writeEn and writeData are literals")
        val prevWriteEns = getVersions(writeEn.asInstanceOf[Wire], writeStage - readStage + 1)
        if (writeStage > readStage) {
          for(stage <- readStage + 1 until writeStage + 1) {
            var currentStageWriteEnable: Bool = null 
            if(-(stage - writeStage)  < prevWriteEns.length){
              currentStageWriteEnable = Bool(module = top)
              currentStageWriteEnable.inputs += prevWriteEns(-(stage - writeStage) )
              prevWriteEns(- (stage - writeStage) ).consumers += ((currentStageWriteEnable, 0))
            } else {
              currentStageWriteEnable = BoolConst(true, module = top).asInstanceOf[Bool]
            }
            regRAWHazards(((reg.readPort, writePort, stage))) = And(stageValids(stage), currentStageWriteEnable, "hazard_num" + raw_counter + "_" + reg.name + "_" + stage + "_" + writeEn.name, top).asInstanceOf[Bool]
            raw_counter = raw_counter + 1
          }
        }
      }
    }
    
    // FunMem hazards
    for (mem <- ArchitecturalMems) {
      for(writePort <- mem.writePorts){
        val writeAddr = writePort.addr
        val writeEn = writePort.en
        val writeData = writePort.data
        val writeStage = Math.max(getStage(writeEn),Math.max(getStage(writeAddr), getStage(writeData)))
        Predef.assert(writeStage > -1)
        for(readPort <- mem.readPorts){
          val readAddr = readPort.addr
          val readData = readPort.data
          val readEn = readPort.en
          val readStage = Math.max(getStage(readAddr), getStage(readEn))
          Predef.assert(readStage > -1)
          val writeEnables = getVersions(writeEn.asInstanceOf[Wire], writeStage - readStage + 1)
          val writeAddrs = getVersions(writeAddr.asInstanceOf[Wire], writeStage - readStage + 1)
          if(writeStage > readStage){
            for(stage <- readStage + 1 until writeStage + 1){
              var currentStageWriteEnable:Bool = null
              var currentStageWriteAddr:Wire = null
              if(-(stage - writeStage)  < writeEnables.length){
                currentStageWriteEnable = Bool(module = top)
                currentStageWriteEnable.inputs += writeEnables(-(stage - writeStage) )
                writeEnables(-(stage - writeStage) ).consumers += ((currentStageWriteEnable, 0))
              } else {
                currentStageWriteEnable = BoolConst(true, module = top).asInstanceOf[Bool]
              }
              if(-(stage - writeStage)  < writeAddrs.length){
                currentStageWriteAddr = Wire(module = top)
                currentStageWriteAddr.inputs += writeAddrs(-(stage - writeStage) )
                writeAddrs(-(stage - writeStage) ).consumers += ((currentStageWriteAddr, 0))
              } else {
                currentStageWriteAddr = readAddr.asInstanceOf[Wire]
              }
              memRAWHazards(((readPort, writePort, stage))) = And(stageValids(stage), And(currentStageWriteEnable, And(readEn.asInstanceOf[Bool], Equal(readAddr.asInstanceOf[Wire], currentStageWriteAddr, module = top), module = top), module = top) , name = "hazard_num" + raw_counter + "_" + mem.name + "readNum_" + mem.readPorts.indexOf(readPort) + "_"+ stage + "_" + "writeNum_" + mem.writePorts.indexOf(writePort), module = top).asInstanceOf[Bool]
              
              raw_counter = raw_counter + 1
            }
          }
        }
      }
    }
  }
   
  def getVersions(wire: Wire, maxLen: Int): ArrayBuffer[Wire] = {
    val result = new ArrayBuffer[Wire]//stage of node decreases as the array index increases; includes original node
    var currentWire:Node = wire
    result += currentWire.asInstanceOf[Wire]
    Predef.assert(currentWire.inputs.length == 1)
    while(currentWire.inputs(0).isInstanceOf[Wire] || currentWire.inputs(0).isInstanceOf[AssignOp] || (currentWire.inputs(0).isInstanceOf[RegRead] && isPipeLineReg(currentWire.asInstanceOf[Wire].getReg))){
      if(currentWire.inputs(0).isInstanceOf[RegRead] && isPipeLineReg(currentWire.asInstanceOf[Wire].getReg)){
        Predef.assert(currentWire.asInstanceOf[Wire].getReg.writePorts.length == 1)
        currentWire = currentWire.asInstanceOf[Wire].getReg.writePorts(0).data
        result += currentWire.asInstanceOf[Wire]
      } else {
        currentWire = currentWire.inputs(0)
      }
      Predef.assert(currentWire.inputs.length == 1)
    }
    
    if(!requireStage(currentWire)){
      for(i <- result.length until maxLen){
        result += currentWire.asInstanceOf[Wire]
      }
    }
    return result
  }
  
  def generateIOBusySignals(): Unit = {
    for(decoupledIO <- ioNodes){
      val stage = getStage(decoupledIO.bits)
      Predef.assert(stage > -1)
      if(decoupledIO.dir == INPUT){
        val readyInput = insertWireOnInput(decoupledIO.ready, 0) 
        decoupledIOBusySignals(((decoupledIO, stage))) = And(Not(decoupledIO.valid.asInstanceOf[Bool], module = top), readyInput, name = decoupledIO.name + "_busy", module = top).asInstanceOf[Bool]
      } else {
        val validInput = insertWireOnInput(decoupledIO.valid, 0)
        decoupledIOBusySignals(((decoupledIO, stage))) = And(validInput, Not(decoupledIO.ready.asInstanceOf[Bool], module = top), name = decoupledIO.name + "_busy", module = top).asInstanceOf[Bool]

      }
    }
    for(varLatIO <- VarLatIOs){
      val stage = getStage(varLatIO.req.bits)
      Predef.assert(stage > -1)
      
      //generate IO busy signal for resp side
      val respReadyInput = insertWireOnInput(varLatIO.resp.ready, 0)
      varLatIOBusySignals(((varLatIO, varLatIO.resp, stage))) = And (Not(varLatIO.resp.valid.asInstanceOf[Bool], module = top), respReadyInput , name = varLatIO.name + "_resp_busy", module = top).asInstanceOf[Bool]

      //generate IO busy signal for req side
      val reqValidInput = insertWireOnInput(varLatIO.req.valid, 0)
      varLatIOBusySignals(((varLatIO, varLatIO.req, stage))) = And (Not(varLatIO.req.ready.asInstanceOf[Bool], module = top), reqValidInput , name = varLatIO.name + "_req_busy", module = top).asInstanceOf[Bool]
    }
  }
  
  def generateStageNoRAWSignals(): Unit = {
    //sort RAW hazard signals by stage
    val RAWHazardSignals = new ArrayBuffer[ArrayBuffer[Bool]]
    for(i <- 0 until pipelineLength){
      RAWHazardSignals += new ArrayBuffer[Bool]
    }
    for(((regRead, regWrite, stage), rawSignal) <- regRAWHazards){
      val readStage = getStage(regRead)
      Predef.assert(readStage > -1)
      Predef.assert(stage > readStage)
      RAWHazardSignals(readStage) += rawSignal
    }

    for(((memRead, memWrite, stage), rawSignal) <- memRAWHazards){
      val readStage = getStage(memRead)
      Predef.assert(readStage > -1)
      Predef.assert(stage > readStage)
      RAWHazardSignals(readStage) += rawSignal
    }
    //generate no raw signals
    for(i <- 0 until pipelineLength){
      var currentStageValid = BoolConst(true, module = top)
      for(rawSignal <- RAWHazardSignals(i)){
        currentStageValid = And(currentStageValid, Not(rawSignal, module = top), module = top)
      }
      Assign(stageNoRAWSignals(i), currentStageValid, module = top)
    }
  }
  
  def generateStageNoIOBusySignals(): Unit = {
    //sort IO busy signals by stage
    val IOBusySignals = new ArrayBuffer[ArrayBuffer[Bool]]
    for(i <- 0 until pipelineLength){
      IOBusySignals += new ArrayBuffer[Bool]
    }
    for(((decoupledIO, stage), ioBusySignal) <- decoupledIOBusySignals){
      IOBusySignals(stage) += ioBusySignal
    }
    for(((varLatIO, decoupledIO, stage), ioBusySignal) <- varLatIOBusySignals){
      IOBusySignals(stage) += ioBusySignal
    }
    
    //generate noIOBusy Signals
    for(i <- 0 until pipelineLength){
      var currentStageValid = BoolConst(true, module = top)
      for(ioBusySignal <- IOBusySignals(i)){
        currentStageValid = And(currentStageValid, Not(ioBusySignal, module = top), module = top)
      }
      Assign(stageNoIOBusySignals(i), currentStageValid, module = top)
    }

  }

  def generateStageValidSignals(): Unit = {
    for(i <- 0 until pipelineLength){
      var currentStageValid = BoolConst(true, module = top)
      currentStageValid = And(currentStageValid, prevStageValidRegs(i), module = top)

      currentStageValid = And(currentStageValid, And(stageNoRAWSignals(i) , stageNoIOBusySignals(i), module = top) , module = top)
      Assign(stageValids(i), currentStageValid, module = top)
    }
  }
 
  def generateStageStallSignals(): Unit = {
    Assign(stageStalls(pipelineLength - 1), BoolConst(false, module = top), module = top)
    for(i <- 0 until pipelineLength - 1){
      val currentStageStall = Or(stageStalls(i+1), And(prevStageValidRegs(i + 1), Or(Not(stageNoRAWSignals(i + 1), module = top), Not(stageNoIOBusySignals(i + 1), module = top) , module = top) , module = top), module = top)
      Assign(stageStalls(i), currentStageStall, module = top)
    }
  }
  
  def connectStageValidsToConsumers(): Unit = {
    //and stage valids to architectural state write enables
    for(reg <- ArchitecturalRegs){
      val writeStage = getStage(reg.writePorts(0))
      Predef.assert(writeStage > -1)
      for(writePort <- reg.writePorts){
        val oldWriteEn = writePort.inputs(0)
        removeFromConsumers(oldWriteEn, writePort)
        val newWriteEn = And(oldWriteEn.asInstanceOf[Bool], stageValids(writeStage), module = top)
        writePort.inputs(0) = newWriteEn
        newWriteEn.consumers += ((writePort, 0))
      }
    }

    for(mem <- ArchitecturalMems){
      val writeStage = getStage(mem.writePorts(0))
      Predef.assert(writeStage > - 1)
      for(writePort <- mem.writePorts){
        val oldWriteEn = writePort.inputs(1)
        removeFromConsumers(oldWriteEn, writePort)
        val newWriteEn = And(oldWriteEn.asInstanceOf[Bool], stageValids(writeStage), module = top)
        writePort.inputs(1) = newWriteEn
        newWriteEn.consumers += ((writePort, 1))
      }
    }
    //and stage valids to input readies/output valids
    for(decoupledIO <- ioNodes){
      val stage = getStage(decoupledIO.bits)
      Predef.assert(stage > -1)
      if(decoupledIO.dir == INPUT){
        val oldReadyInput = insertWireOnInput(decoupledIO.ready, 0)
        removeFromConsumers(oldReadyInput, decoupledIO.ready)
        val newReadyInput = And(oldReadyInput.asInstanceOf[Bool], And(stageNoRAWSignals(stage), prevStageValidRegs(stage), module = top), module = top)
        decoupledIO.ready.inputs(0) = newReadyInput
        newReadyInput.consumers += ((decoupledIO.ready, 0))
      } else {
        val oldValidInput = insertWireOnInput(decoupledIO.valid, 0)
        removeFromConsumers(oldValidInput, decoupledIO.valid)
        val newValidInput = And(oldValidInput.asInstanceOf[Bool], And(stageNoRAWSignals(stage), prevStageValidRegs(stage), module = top), module = top)
        decoupledIO.valid.inputs(0) = newValidInput
        newValidInput.consumers += ((decoupledIO.valid, 0))
      }
    }
    for(varLatIO <- VarLatIOs){
      val stage = getStage(varLatIO.req.bits)
      Predef.assert(stage > - 1)
      
      val oldReadyInput = insertWireOnInput(varLatIO.resp.ready, 0)
      removeFromConsumers(oldReadyInput, varLatIO.resp.ready)
      val newReadyInput = And(oldReadyInput.asInstanceOf[Bool], And(stageNoRAWSignals(stage), prevStageValidRegs(stage), module = top), module = top)
      varLatIO.resp.ready.inputs(0) = newReadyInput
      newReadyInput.consumers += ((varLatIO.resp.ready, 0))

      val oldValidInput = insertWireOnInput(varLatIO.req.valid, 0)
      removeFromConsumers(oldValidInput, varLatIO.req.valid)
      val newValidInput = And(oldValidInput.asInstanceOf[Bool], And(stageNoRAWSignals(stage), prevStageValidRegs(stage), module = top), module = top)
      varLatIO.req.valid.inputs(0) = newValidInput
      newValidInput.consumers += ((varLatIO.req.valid, 0))

    }
  }

  def connectStageStallsToConsumers(): Unit = {
    //and ~stage stalls to architectural state write enables
    for(reg <- ArchitecturalRegs){
      val writeStage = getStage(reg.writePorts(0))
      Predef.assert(writeStage > -1)
      for(writePort <- reg.writePorts){
        val oldWriteEn = writePort.inputs(0)
        removeFromConsumers(oldWriteEn, writePort)
        val newWriteEn = And(oldWriteEn.asInstanceOf[Bool], Not(stageStalls(writeStage), module = top), module = top)
        writePort.inputs(0) = newWriteEn
        newWriteEn.consumers += ((writePort, 0))
      }
    }

    for(mem <- ArchitecturalMems){
      val writeStage = getStage(mem.writePorts(0))
      Predef.assert(writeStage > - 1)
      for(writePort <- mem.writePorts){
        val oldWriteEn = writePort.inputs(1)
        removeFromConsumers(oldWriteEn, writePort)
        val newWriteEn = And(oldWriteEn.asInstanceOf[Bool], Not(stageStalls(writeStage), module = top), module = top)
        writePort.inputs(1) = newWriteEn
        newWriteEn.consumers += ((writePort, 1))
      }
    }
    //and ~stage stalls to input readies/output valids
    for(decoupledIO <- ioNodes){
      val stage = getStage(decoupledIO.bits)
      Predef.assert(stage > -1)
      if(decoupledIO.dir == INPUT){
        val oldReadyInput = insertWireOnInput(decoupledIO.ready, 0)
        removeFromConsumers(oldReadyInput, decoupledIO.ready)
        val newReadyInput = And(oldReadyInput.asInstanceOf[Bool], Not(stageStalls(stage), module = top), module = top)
        decoupledIO.ready.inputs(0) = newReadyInput
        newReadyInput.consumers += ((decoupledIO.ready, 0))
      } else {
        val oldValidInput = insertWireOnInput(decoupledIO.valid, 0)
        removeFromConsumers(oldValidInput, decoupledIO.valid)
        val newValidInput = And(oldValidInput.asInstanceOf[Bool], Not(stageStalls(stage), module = top), module = top)
        decoupledIO.valid.inputs(0) = newValidInput
        newValidInput.consumers += ((decoupledIO.valid, 0))
      }
    }
    for(varLatIO <- VarLatIOs){
      val stage = getStage(varLatIO.req.bits)
      Predef.assert(stage > - 1)
      
      val oldReadyInput = insertWireOnInput(varLatIO.resp.ready, 0)
      removeFromConsumers(oldReadyInput, varLatIO.resp.ready)
      val newReadyInput = And(oldReadyInput.asInstanceOf[Bool], Not(stageStalls(stage), module = top), module = top)
      varLatIO.resp.ready.inputs(0) = newReadyInput
      newReadyInput.consumers += ((varLatIO.resp.ready, 0))

      val oldValidInput = insertWireOnInput(varLatIO.req.valid, 0)
      removeFromConsumers(oldValidInput, varLatIO.req.valid)
      val newValidInput = And(oldValidInput.asInstanceOf[Bool], Not(stageStalls(stage), module = top), module = top)
      varLatIO.req.valid.inputs(0) = newValidInput
      newValidInput.consumers += ((varLatIO.req.valid, 0))
    }

    //and ~stage stalls to pipeline register enables
    for(i <- 0 until pipelineLength - 1){
      for(pipelineReg <- pipelineRegs(i)){
        Predef.assert(pipelineReg.writePorts.length == 1)
        val writePort = pipelineReg.writePorts(0)
        val writeStage = i
        val oldWriteEn = writePort.inputs(0)
        removeFromConsumers(oldWriteEn, writePort)
        val newWriteEn = And(oldWriteEn.asInstanceOf[Bool], Not(stageStalls(writeStage), module = top), module = top)
        writePort.inputs(0) = newWriteEn
        newWriteEn.consumers += ((writePort, 0))
      }
    }

    //and ~stage stalls to stage valid register enables
    for(i <- 1 until pipelineLength){
      Predef.assert(prevStageValidRegs(i).getReg.writePorts.length == 1)
      val writePort = prevStageValidRegs(i).getReg.writePorts(0)
      val writeStage = i - 1
      val oldWriteEn = writePort.inputs(0)
      removeFromConsumers(oldWriteEn, writePort)
      val newWriteEn = And(oldWriteEn.asInstanceOf[Bool], Not(stageStalls(writeStage), module = top), name = "Stage_" + i + "_valid_reg_en", module = top)
      writePort.inputs(0) = newWriteEn
      newWriteEn.consumers += ((writePort, 0))
    }
  }

  def removeFromConsumers(node: Node, consumerToBeRemoved: Node): Unit = {
    val newConsumers = new ArrayBuffer[(Node, Int)]
    for((consumer, index) <- node.consumers){
      if(consumer != consumerToBeRemoved){
        newConsumers += ((consumer, index))
      }
    }
    node.consumers.clear()
    for((consumer, index) <- newConsumers){
      node.consumers += ((consumer, index))
    }
  }

  def insertAssignOpsBetweenWires() : Unit = {
    for(node <- top.nodes.filter(_.isInstanceOf[Wire])){
      if(node.inputs.length == 1 && node.inputs(0).isInstanceOf[Wire]){
        Predef.assert(node.inputs(0).getClass == node.getClass)
        val assignOp = new AssignOp
        assignOp.inputs += node.inputs(0)
        for(i <-0 until node.inputs(0).consumers.length){
          val consumer = node.inputs(0).consumers(i)._1
          if(consumer == node){
            node.inputs(0).consumers(i) = ((assignOp, 0))
          }
        }

        assignOp.module = top
        top.nodes += assignOp

        node.inputs(0) = assignOp
        assignOp.consumers += ((node, 0))
      }
    }
  }

}
