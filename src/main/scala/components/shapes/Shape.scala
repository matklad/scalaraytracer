package components.shapes

import data.Types._
import components.Material

abstract class Shape(val material: Material) {

  def normalAt(p: P): D

  def intersectWith(r: R): S
}
