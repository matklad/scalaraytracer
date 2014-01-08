package components.geometry
import data.Types._

trait Index[T <: Shape] {
  def intersect(ray: R) : (S, T)
}
