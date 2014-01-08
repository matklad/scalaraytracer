package components.geometry

import data.Types._


class Triangle(val a: P, val b: P, val c: P) extends Shape {

  protected val (n: D, abXnN: V, acXnN: V, ab: V, ac: V) = {
    val ab = b - a
    val ac = c - a
    val n = (ab cross ac).direction
    val abXn = ab cross n
    val acXn = ac cross n
    val abDacXn = ab dot acXn
    val acDabXn = ac dot abXn
    val abXnN = abXn * (1 / acDabXn)
    val acXnN = acXn * (1 / abDacXn)
    (n, abXnN, acXnN, ab, ac)
  }

  def normalAt(p: P): D = n

  def intersectWith(r: R): S = {
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
        S.MaxValue
    } else
      S.MaxValue
  }
}

object Triangle {
  def apply(a: P, b: P, c: P) =
    new Triangle(a, b, c)
}

class InterpolatedTriangle(a: P, val na: D, b: P, val nb: D, c: P, val nc: D)
  extends Triangle(a, b, c) {
  override def normalAt(p: P): D = {
    // a + x * ab + y * ac = p
    // let v = p - a
    // x * ab + y * ac = v
    val v = p - a
    val oac = ab - ac * ((ac dot ab) / (ac dot ac))
    val oab = ac - ab * ((ac dot ab) / (ab dot ab))
    val x = (v dot oac) / (ab dot oac)
    val y = (v dot oab) / (ac dot oab)
    val z = 1 - (x + y)
    val n = nb * x + nc * y + na * z
    n.direction
  }
}

object InterpolatedTriangle {
  def apply(a: P, na: D, b: P, nb: D, c: P, nc: D) =
    new InterpolatedTriangle(a, na, b, nb, c, nc)
}