package data


class BitMap private(val width: Int, val height: Int, private val data: Array[Array[Color]]) {
  def apply(x: Int, y: Int) = data(x)(y)

  def downSample: BitMap = {
    BitMap(width / 2, height / 2) {
      (x, y) => (this(2 * x, 2 * y) + this(2 * x + 1, 2 * y) +
        this(2 * x, 2 * y + 1) + this(2 * x + 1, 2 * y + 1)).amplify(.25)
    }
  }
}

object BitMap {
  def apply(width: Int = 640, height: Int = 480)(f: (Int, Int) => Color): BitMap = {
    val data = Array.ofDim[Color](width, height)
    for (x <- 0 until width; y <- 0 until height)
      data(x)(y) = f(x, y).norm
    new BitMap(width, height, data)
  }
}
