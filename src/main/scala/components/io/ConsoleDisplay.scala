package components.io

trait ConsoleDisplay extends Display {
  val white = ' '
  val black = '.'

  def display(bitmap: BitMap): Unit = {
    var ans = new StringBuilder()
    for (y <- 0 until bitmap.height) {
      for (x <- 0 until bitmap.width) {
        ans += (if (bitmap(x, y).norm.value < .5) black else white)
      }
      ans += '\n'
    }
    println(ans.toString())
  }
}