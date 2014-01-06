package components


class Material(val ambientK: Double, val diffuseK: Double, val specularK: Double) {

}

object Material {
  def apply(ambientK: Double = .9, diffuseK: Double = .9, specularK: Double = 4): Material = {
    new Material(ambientK, diffuseK, specularK)
  }

  val simple = Material()
  val absoluteBlack = Material(ambientK = 0, diffuseK = 0, specularK = 1e18)
}