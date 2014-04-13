package hdl

object hdlMain {
  def apply[T <: Module](top: T, fileName: String, packageName: String): Unit = {
    codeGenerator(top, fileName, packageName)
  }
}
