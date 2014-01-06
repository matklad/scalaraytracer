package components.scene

import data.Types._

class Camera private(val position: P, val direction: D, val up: D, val right: D,
                     val focus: S, val screenSize: (S, S),
                     val resolution: (Int, Int)) {
  val center = position + direction * focus
  val maxX = resolution._1 - 1
  val maxY = resolution._2 - 1
  val dx = right * (screenSize._1 / maxX)
  val dy = up * (-screenSize._2 / maxY)
  assert((direction cross up) == right)

  def apply(x: Int, y: Int) = {
    assert(0 <= x && x <= maxX && 0 <= y && y <= maxY)
    val p = center + dx * (x - maxX / 2.0) + dy * (y - maxY / 2.0)
    R(position, p)
  }
}

object Camera {
  def apply(position: P, lookAt: P, up: D,
            focus: S, screenSize: (S, S),
            resolution: (Int, Int)) = {
    val direction = (lookAt - position).direction
    val right = (direction cross up).direction
    val fixedUp = (right cross direction).direction
    new Camera(position, direction, fixedUp, right,
      focus, screenSize, resolution)
  }
}
