package components.geometry

import data.Types._

class Sphere(val radius: S, val center: P) extends Shape {
  assert(radius > 0)

  def normalAt(p: P): D = {
    val r = p - center
    assert(r.length - radius < 1e-6)
    r.direction
  }

  def intersectWith(r: R): S = {
    /*
    (center - X)^2 = radius^2
    origin + direction * t = X
    --------------------------
    ler center - origin = s
        (s, direction) = sd in
    (s - direction * t)^2 = radius^2
    t^2 * direction^2 - 2 * sd *t + d^2 - radius^2 == 0
    D' = sd ^ 2 - direction^2 * (s^2 - radius^2)
    t = (sd +- sqrt(D')) / direction^2
    */
    val s = center - r.origin
    val ss = s dot s
    val sd = s dot r.direction
    val dd = r.direction dot r.direction
    val D = sd * sd - dd * (ss - radius * radius)
    if (D < 0)
      noIntersection
    else {
      val sqrtD = math.sqrt(D)
      val (t1, t2) = ((sd - sqrtD) / dd, (sd + sqrtD) / dd)

      if (t1 > 0)
        t1
      else if (t2 > 0)
        t2
      else
        noIntersection
    }
  }

}

object Sphere {
  def apply(radius: S, center: P = P.origin): Sphere =
    new Sphere(radius, center)
}