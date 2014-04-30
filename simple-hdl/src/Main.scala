package hdl

object hdlMain {
  def apply[T <: Module](top: T, fileName: String, packageName: String): Unit = {
    top.isTop = true
    autoMultiThread(top)
    codeGenerator(top, fileName, packageName)
  }
}
