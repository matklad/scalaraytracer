package data

import scala.reflect.ClassTag

class BitMap[C] private(val width: Int, val height: Int, private val data: Array[Array[C]]) {
  def apply(x: Int, y: Int) = data(x)(y)
}

object BitMap {
  def apply[C: ClassTag](width: Int = 640, height: Int = 480, f: (Int, Int) => C): BitMap[C] = {
    val data = Array.ofDim[C](width, height)
    for (x <- 0 until width; y <- 0 until height)
      data(x)(y) = f(x, y)
    new BitMap[C](width, height, data)
  }
}
