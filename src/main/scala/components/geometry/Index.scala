package components.geometry

import data.Types._

trait Index {
  def intersect(r: R): (S, Shape)
}