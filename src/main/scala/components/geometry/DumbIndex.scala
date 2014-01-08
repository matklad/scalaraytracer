package components.geometry

import data.Types._


class DumbIndex[T <: Shape](val items: Vector[T], val default: T) extends Index[T] {
  def intersect(ray: R): (S, T) = {
    var intersection: (S, T) = (default.intersectWith(ray), default)
    items foreach {
      s =>
        val t = s.intersectWith(ray)
        if (t < intersection._1)
          intersection = (t, s)
    }
    assert(intersection._1 > 0)
    intersection
  }
}

object DumbIndex {
  def apply[T <: Shape](items: Vector[T], default: T) =
    new DumbIndex[T](items, default)
}