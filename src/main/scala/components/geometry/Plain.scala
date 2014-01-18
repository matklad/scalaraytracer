package components.geometry

import data.Types._
import data.Types

class Plain(val origin: P, val ox: D, val oy: D) extends Shape {
  val n = (ox cross oy).direction

  def normalAt(p: P): D = n

  def intersectWith(r: R): S = {
    // (n, (X - origin)) == 0
    // X = r.origin +  t * r.direction
    // t = (n, origin - r.origin) / (n, r.direction)
    val t = (n dot (origin - r.origin)) / (n dot r.direction)
    if (t > 0) t else noIntersection
  }

  override def coordinatesAt(p: Types.P): (Types.S, Types.S) = {
    val v = p - origin
    (v dot ox, v dot oy)
  }

}

object Plain {
  def apply(origin: P = P.origin, ox: D = V.i, oy: D = V.j) =
    new Plain(origin, ox, oy)
}