package components.geometry

import data.Types._


trait DumbIndex extends IndexWrapper {

  private class Dumb(val items: Iterable[Shape], val default: Shape) extends Index {
    def intersect(ray: R): (S, Shape) = {
      var intersection: (S, Shape) = (default.intersectWith(ray), default)
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


  def createIndex(shapes: Iterable[Shape], default: Shape): Index =
    new Dumb(shapes, default)
}

