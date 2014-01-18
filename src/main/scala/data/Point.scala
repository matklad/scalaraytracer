package data


class Point private(val v: Vector3d) extends AnyVal {
  def -(that: Point): Vector3d = v - that.v

  def +(that: Vector3d): Point = new Point(v + that)

  override def toString: String = {
    s"$v"
  }
}

object Point {
  def apply(v: Vector3d) = new Point(v)

  def apply(x: Double, y: Double, z: Double) = new Point(Vector3d(x, y, z))

  def origin = new Point(Vector3d.zero)

  def lowerBound(p1: Point, p2: Point) = new Point(Vector3d.lowerBound(p1.v, p2.v))

  def upperBound(p1: Point, p2: Point) = new Point(Vector3d.upperBound(p1.v, p2.v))
}
