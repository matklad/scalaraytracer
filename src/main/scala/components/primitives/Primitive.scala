package components.primitives

import components.geometry.Shape
import data.Types._

class Primitive private(private val shape: Shape,
                        private val texture: Texture,
                        val material: Material) {

  def normalAt(p: P): D = shape.normalAt(p)

  def intersectWith(r: R): S = shape.intersectWith(r)

  def colorAt(p: P): Color = {
    val (x, y) = shape.coordinatesAt(p)
    texture.colorAt(x, y)
  }
}


object Primitive {
  def apply(shape: Shape, texture: Texture, material: Material) =
    new Primitive(shape, texture, material)
}