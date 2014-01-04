package components.shapes

import data.Data._
import components.Material


class Triangle(val a: P, val b: P, val c: P, material: Material) extends Shape(material) {

  private val (n: D, abXnN: V, acXnN: V) = {
    val ab = b - a
    val ac = c - a
    val n = (ab cross ac).direction
    val abXn = ab cross n
    val acXn = ac cross n
    val abDacXn = ab dot acXn
    val acDabXn = ac dot abXn
    val abXnN = abXn * (1 / acDabXn)
    val acXnN = acXn * (1 / abDacXn)
    (n, abXnN, acXnN)
  }

  def normalAt(p: P): D = n

  def intersectWith(r: R): Double = {
    val ao = a - r.origin
    val denom = r.direction dot n
    if (math.abs(denom) > 1e-6) {
      val t = (ao dot n) / denom
      val tdo = r.direction * t - ao
      val p = tdo dot acXnN
      val q = tdo dot abXnN
      if (0 < p && p < 1 && 0 < q && q < 1 && p + q < 1)
        t
      else
        Double.MaxValue
    } else
      Double.MaxValue
  }
}

object Triangle {
  def apply(a: P, b: P, c: P, material: Material) = new Triangle(a, b, c, material)
}