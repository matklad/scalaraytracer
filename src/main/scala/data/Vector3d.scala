package data

import spire.implicits._
import spire.math._

class Vector3d[T: Fractional](val x: T, val y: T, val z: T) {

  private[data] def map(f: T => T): Vector3d[T] = new Vector3d(f(x), f(y), f(z))

  private[data] def zipWith(f: (T, T) => T)(that: Vector3d[T]): Vector3d[T] = new Vector3d(f(x, that.x), f(y, that.y), f(z, that.z))

  def +(that: Vector3d[T]): Vector3d[T] = zipWith(_ + _)(that)

  def -(that: Vector3d[T]): Vector3d[T] = zipWith(_ - _)(that)

  def unary_- = map(-_)

  def *(that: T) = map(_ * that)

  def dot(that: Vector3d[T]): T = {
    val r = this.zipWith(_ * _)(that)
    r.x + r.y + r.z
  }

  def cross(that: Vector3d[T]): Vector3d[T] = {
    val (a, b, c) = (that.x, that.y, that.z)
    // x y z
    // a b c
    new Vector3d(y * c - z * b, -(x * c - z * a), x * b - y * a)
  }

  def length: T = sqrt(x * x + y * y + z * z)

  def direction: UnitVector3d[T] = {
    val u = this * (1 / this.length)
    new UnitVector3d[T](u.x, u.y, u.z)
  }

  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: Vector3d[T] => (this - that).length < 1e-6
      case _ => false
    }
  }

  override def toString: String = {
    s"($x, $y, $z)"
  }
}

class UnitVector3d[T](x: T, y: T, z: T)(implicit ev: Fractional[T]) extends Vector3d[T](x, y, z) {
  assert(abs(length - ev.one) < 1e-6, s"$length ${ev.one}")

  override def unary_- = new UnitVector3d[T](-x, -y, -z)

}

object Vector3d {
  def apply[T: Fractional](x: T, y: T, z: T): Vector3d[T] = {
    new Vector3d[T](x, y, z)
  }

  def zero[T](implicit ev: Fractional[T]): Vector3d[T] = Vector3d(ev.zero, ev.zero, ev.zero)

  def i[T](implicit ev: Fractional[T]): UnitVector3d[T] = new UnitVector3d(ev.one, ev.zero, ev.zero)

  def j[T](implicit ev: Fractional[T]): UnitVector3d[T] = new UnitVector3d(ev.zero, ev.one, ev.zero)

  def k[T](implicit ev: Fractional[T]): UnitVector3d[T] = new UnitVector3d(ev.zero, ev.zero, ev.one)

}

