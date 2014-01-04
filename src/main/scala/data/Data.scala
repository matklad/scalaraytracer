package data

trait Data {

  type S = Double
  val S = Double
  type V = Vector3d[S]

  object V {
    def apply(x: S, y: S, z: S) = Vector3d.apply[S](x, y, z)

    val zero = Vector3d.zero[S]
    val i = Vector3d.i[S]
    val j = Vector3d.j[S]
    val k = Vector3d.k[S]
  }

  type D = UnitVector3d[S]
  type P = Point[S]
  type PT = (S, S, S)

  object P {
    def apply(v: V) = Point[S](v)
    def apply(x: S, y: S, z: S) = Point[S](x, y, z)
    val origin = Point.origin[S]
  }

  type R = Ray[S]

  object R {
    def apply(o: P, d: D) = Ray[S](o, d)

    def apply(from: P, to: P) = Ray[S](from, to)
  }

  type BitMap = data.BitMap[Color]

  object BitMap {
    def apply(width: Int = 640, height: Int = 480)(f: (Int, Int) => Color) =
      data.BitMap[Color](width, height, f)
  }

  type Color = data.Color
  val Color = data.Color

}

object Data extends Data