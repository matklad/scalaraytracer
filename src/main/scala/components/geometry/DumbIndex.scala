package components.geometry

import data.Types._


class DumbIndex(val items: Iterable[Shape]) extends Index {
  def intersect(ray: R): Intersection =
    (items.view map (Intersection(ray, _))).fold(Intersection.zero)(_ >< _)
}

object DumbIndex {
  def apply(items: Iterable[Shape]): DumbIndex =
    new DumbIndex(items)
}