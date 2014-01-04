package data


class Point(val v: Vector3d) extends AnyVal {
  def -(that: Point): Vector3d = v - that.v

  def +(that: Vector3d): Point = new Point(v + that)
}

object Point {
  def apply(v: Vector3d) = new Point(v)

  def apply(x: Double, y: Double, z: Double) = new Point(Vector3d(x, y, z))

  def origin = new Point(Vector3d.zero)
}
