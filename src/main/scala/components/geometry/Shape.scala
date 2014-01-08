package components.geometry

import data.Types._

trait Shape {

  def normalAt(p: P): D

  def intersectWith(r: R): S

  def coordinatesAt(p: P): (S, S) = (0, 0)
}
