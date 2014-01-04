package data

class Ray(val origin: Point, val direction: UnitVector3d) {
  def along(t: Double): Point = origin + direction * t
}

object Ray {
  def apply(origin: Point, direction: UnitVector3d): Ray = new Ray(origin, direction)

  def apply(from: Point, to: Point): Ray = Ray(from, (to - from).direction)
}


