package components.geometry

import data.Types._

trait IndexWrapper {
  def createIndex(shapes: Iterable[Shape], default: Shape): Index
}

trait Index {
  def intersect(r: R): (S, Shape)
}