package data

class Ray[T](val origin: Point[T], val direction: UnitVector3d[T]) {
  def along(t: T): Point[T] = origin + direction * t
}

object Ray {
  def apply[T](origin: Point[T], direction: UnitVector3d[T]): Ray[T] = new Ray[T](origin, direction)

  def apply[T](from: Point[T], to: Point[T]): Ray[T] = Ray(from, (to - from).direction)
}


