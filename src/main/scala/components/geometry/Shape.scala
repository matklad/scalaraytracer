package components.geometry

import data.Types._

abstract class Shape {

  val noIntersection = S.MaxValue

  def normalAt(p: P): D

  def intersectWith(r: R): S

  def coordinatesAt(p: P): (S, S) = (0, 0)

  override final def equals(other: Any) = other match {
    case s: Shape => eq(s)
    case _ => false
  }
}
