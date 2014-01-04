package components

import data._

class Material(val color: Color, val ambientK: Double, val diffuseK: Double, val specularK: Double) {

}

object Material {
  def apply(color: Color, ambientK: Double = .9, diffuseK: Double = .9, specularK: Double = 4): Material = {
    new Material(color, ambientK, diffuseK, specularK)
  }

  val simple = Material(Color.white)
  val absoluteBlack = Material(Color.pureBlack, ambientK = 0, diffuseK = 0, specularK = 1e18)
}