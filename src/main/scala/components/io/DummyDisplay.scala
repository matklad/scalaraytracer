package components.io

import data.{Color, BitMap}

class DummyDisplay extends Display {
  def display(bitmap: BitMap): Unit = {
    var totalRed = 0
    for (x <- 0 until bitmap.width; y <- 0 until bitmap.height) {
      val Color.RGB(r, g, b) = bitmap(x, y)
      totalRed += r
    }
    println(totalRed - 13537646)
  }
}
