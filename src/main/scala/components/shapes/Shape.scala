package components.shapes

import data.Types._
import components.{Texture, Material}

abstract class Shape(val material: Material, val texture: Texture) {

  def normalAt(p: P): D

  def colorAt(p: P): Color = {
    val (x, y) = coordinatesAt(p)
    texture.colorAt(x, y)
  }

  def intersectWith(r: R): S

  private def coordinatesAt(p: P): (S, S) = (0, 0)
}
