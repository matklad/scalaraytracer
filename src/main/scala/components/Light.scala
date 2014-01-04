package components

import data.Data._

class LightRay(val color: Color, val ray: R)

class LightSource(val position: P, val color: Color) {
  def shed(point: P): LightRay = {
    new LightRay(color, R(position, point))
  }
}

object LightSource {
  def apply(position: P, color: Color): LightSource = {
    new LightSource(position, color)
  }
}
