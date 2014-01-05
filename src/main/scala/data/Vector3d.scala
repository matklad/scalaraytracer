package data


class Vector3d protected(val x: Double, val y: Double, val z: Double) {

  private[data] def map(f: Double => Double): Vector3d = new Vector3d(f(x), f(y), f(z))

  private[data] def zipWith(f: (Double, Double) => Double)(that: Vector3d): Vector3d = new Vector3d(f(x, that.x), f(y, that.y), f(z, that.z))

  def +(that: Vector3d): Vector3d = zipWith(_ + _)(that)

  def -(that: Vector3d): Vector3d = zipWith(_ - _)(that)

  def unary_- = map(-_)

  def *(that: Double) = map(_ * that)

  def dot(that: Vector3d): Double = {
    val r = this.zipWith(_ * _)(that)
    r.x + r.y + r.z
  }

  def cross(that: Vector3d): Vector3d = {
    val (a, b, c) = (that.x, that.y, that.z)
    // x y z
    // a b c
    new Vector3d(y * c - z * b, -(x * c - z * a), x * b - y * a)
  }

  def length: Double = Math.sqrt(x * x + y * y + z * z)

  def direction: UnitVector3d = {
    val u = this * (1 / this.length)
    new UnitVector3d(u.x, u.y, u.z)
  }

  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: Vector3d => (this - that).length < 1e-6
      case _ => false
    }
  }

  override def toString: String = {
    s"($x, $y, $z)"
  }
}

class UnitVector3d(x: Double, y: Double, z: Double) extends Vector3d(x, y, z) {
  assert(Math.abs(length - 1) < 1e-6, s"$length ${1}")

  override def unary_- = new UnitVector3d(-x, -y, -z)

}

object Vector3d {
  def apply(x: Double, y: Double, z: Double): Vector3d = {
    new Vector3d(x, y, z)
  }

  val zero: Vector3d = Vector3d(0, 0, 0)

  val i: UnitVector3d = new UnitVector3d(1, 0, 0)

  val j: UnitVector3d = new UnitVector3d(0, 1, 0)

  val k: UnitVector3d = new UnitVector3d(0, 0, 1)

}

