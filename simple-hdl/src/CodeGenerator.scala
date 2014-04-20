package hdl
import scala.collection.mutable._

object codeGenerator {
  val emittedModules = new HashSet[String]
  def apply[T <: Module] (top: T, fileName: String, packageName: String): Unit = {
    val chiselSrcFile = new java.io.FileWriter(fileName)
    emitHeader(packageName, chiselSrcFile)
    emitModule(top, chiselSrcFile) 
    chiselSrcFile.close
  }

  def emitHeader(packageName: String, outFile: java.io.FileWriter): Unit = {
    outFile.write("package " + packageName +"\n")
    outFile.write("import Chisel._\n")
    outFile.write("import Common._\n")
  }

  def emitModule(module: Module, outFile: java.io.FileWriter): Unit = {
    module.emitChiselSrc(outFile)
    for(child <- module.children){
      emitModule(child, outFile)
    }
  }
}
