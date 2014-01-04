package data


object Types {

  type S = Double
  val S = Double

  type V = Vector3d
  val V = Vector3d

  type D = UnitVector3d
  type P = Point
  val P = Point


  type R = Ray
  val R = Ray

  type BitMap = data.BitMap[Color]

  object BitMap {
    def apply(width: Int = 640, height: Int = 480)(f: (Int, Int) => Color) =
      data.BitMap[Color](width, height, f)
  }

  type Color = data.Color
  val Color = data.Color

}
