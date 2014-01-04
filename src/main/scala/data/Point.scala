package data

import spire.math.Fractional

class Point[T](val v: Vector3d[T]) extends AnyVal {
  def -(that: Point[T]): Vector3d[T] = v - that.v

  def +(that: Vector3d[T]): Point[T] = new Point[T](v + that)
}

object Point {
  def apply[T: Fractional](v: Vector3d[T]) = new Point[T](v)

  def apply[T: Fractional](x: T, y: T, z: T) = new Point[T](Vector3d[T](x, y, z))

  def origin[T: Fractional] = new Point[T](Vector3d.zero[T])
}
