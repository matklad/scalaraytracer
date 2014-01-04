package components

import data.Data._

class Material(val color: Color, val ambientK: S, val diffuseK: S, val specularK: S) {

}

object Material {
  def apply(color: Color, ambientK: S = .9, diffuseK: S = .9, specularK: S = 4): Material = {
    new Material(color, ambientK, diffuseK, specularK)
  }

  val simple = Material(Color.white)
  val absoluteBlack = Material(Color.pureBlack, ambientK = 0, diffuseK = 0, specularK = 1e18)
}