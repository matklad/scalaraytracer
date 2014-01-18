package components.geometry

import data.Types._

final class Intersection(val t: S, val shape: Shape) {
  assert(t > 0)

  def ><(other: Intersection) =
    if (t < other.t) this else other

}

object Intersection {
  def apply(t: S, shape: Shape) = new Intersection(t, shape)

  def apply(r: R, shape: Shape) = new Intersection(shape.intersectWith(r), shape)

  def zero = Intersection(S.MaxValue - 1, Sphere(10e18))
}
