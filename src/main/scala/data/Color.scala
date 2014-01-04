package data

class Color(val v: Vector3d) extends AnyVal{
  def +(that: Color): Color = new Color(v + that.v)

  def amplify(t: Double): Color = new Color(v * t)

  def *(that: Color) = new Color(v.zipWith(_ * _)(that.v))

  def norm = {
    new Color(v map (x => math.max(0, math.min(1, x))))
  }

  def r = (v.x * 255).toInt

  def g = (v.y * 255).toInt

  def b = (v.z * 255).toInt

  def value = (r + g + b) / 3

  override def toString: String = f"[$r%3d $g%3d $b%3d]"
}

object Color {
  def apply(r: Double, g: Double, b: Double): Color = {
    new Color(Vector3d(r, g, b))
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
}
