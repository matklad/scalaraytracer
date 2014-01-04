package components.scene

import data.Types._

class Camera(val position: P, val direction: D, val up: D, val right: D,
             val focus: S, val width: S, val height: S,
             val resolutionX: Int, val resolutionY: Int) {
  val center = position + direction * focus
  val maxX = resolutionX - 1
  val maxY = resolutionY - 1
  val dx = right * (width / maxX)
  val dy = up * (-height / maxY)
  assert((direction cross up) == right)

  def apply(x: Int, y: Int) = {
    assert(0 <= x && x <= maxX && 0 <= y && y <= maxY)
    val p = center + dx * (x - maxX / 2.0) + dy * (y - maxY / 2.0)
    R(position, p)
  }
}

object Camera {
  def apply(position: P, lookAt: P = P.origin, up: D = V.k,
            focus: S, width: S, height: S,
            resolutionX: Int = 640, resolutionY: Int = 480) = {
    val direction = (lookAt - position).direction
    val right = (direction cross up).direction
    val fixedUp = (right cross direction).direction
    new Camera(position, direction, fixedUp, right,
      focus, width, height, resolutionX, resolutionY)
  }
}
