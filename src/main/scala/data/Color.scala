package data

class Color private(private val r: Double, private val g: Double, private val b: Double) {
  def +(that: Color): Color = new Color(r + that.r, g + that.g, b + that.b)

  def amplify(t: Double): Color = new Color(r * t, g * t, b * t)

  def *(that: Color) = new Color(r * that.r, g * that.g, b * that.b)

  def norm = {
    val f: Double => Double = x => math.max(0, math.min(1, x))
    new Color(f(r), f(g), f(b))
  }

  def value = (r + g + b) / 3

  override def toString: String = this match {
    case Color.RGB(rr, gg, bb) => f"[$rr%3d $gg%3d $bb%3d]"
  }
}

object Color {
  def apply(r: Double, g: Double, b: Double): Color = {
    new Color(r, g, b)
  }

  private val z = 0.05

  val white = Color(1, 1, 1)
  val black = Color(z, z, z)
  val pureBlack = Color(0, 0, 0)
  val zero = pureBlack

  val red = Color(.7, z, z)
  val pureRed = Color(1, 0, 0)

  val green = Color(z, .7, z)
  val pureGreen = Color(0, 1, 0)

  val blue = Color(z, z, .7)
  val pureBlue = Color(0, 0, 1)

  object RGB {
    def unapply(c: Color): Option[(Int, Int, Int)] = {
      val n = c.norm
      def f(x: Double): Int = (x * 255).toInt
      Some(f(n.r), f(n.g), f(n.b))
    }
  }

}