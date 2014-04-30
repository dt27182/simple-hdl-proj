package hdl
import scala.math._

object log2Up
{
  def apply(in: Int): Int = if(in == 1) 1 else ceil(log(in)/log(2)).toInt
}
