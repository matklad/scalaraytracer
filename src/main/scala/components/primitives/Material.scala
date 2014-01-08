package components.primitives

class Material(val ambientK: Double, val diffuseK: Double, val specularK: Double,
               val reflectK: Double, val opacityK: Double) {

}

object Material {
  def apply(ambientK: Double = .9, diffuseK: Double = .9, specularK: Double = 4,
            reflectK: Double = .1, opacityK: Double = .9): Material = {
    new Material(ambientK, diffuseK, specularK, reflectK, opacityK)
  }

  val simple = Material()
  val badMirror = Material(reflectK = .4, opacityK = .6)
  val mirror = Material(0, 0, 5, 1, 0)
  val absoluteBlack = Material(ambientK = 0, diffuseK = 0, specularK = 1e18)
}