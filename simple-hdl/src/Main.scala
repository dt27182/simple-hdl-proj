package hdl

object hdlMain {
  def apply[T <: Module](top: T, fileName: String, packageName: String): Unit = {
    top.isTop = true
    autoPipeline(top)
    codeGenerator(top, fileName, packageName)
  }
}
