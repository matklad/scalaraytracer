package components.io

import components.geometry.{Triangle, Shape}
import data.Types._

object ObjParser {
  def parse(input: String): Vector[Shape] = {
    val lines = input.lines.toArray

    def selectLines(start: String): Array[String] =
      lines filter (_.startsWith(start + " ")) map (_.drop(start.length + 1).trim)

    val vLines = selectLines("v")
    val fLines = selectLines("f")

    def readVertex(line: String): P = {
      val Array(x, y, z) = line split " " map (_.toDouble)
      P(x, y, z)
    }
    val vertices = vLines map readVertex

    def readFace(line: String): Triangle = {
      val blocks = line split " "
      val Array(v1, v2, v3) = blocks map (x => vertices(x.split("/")(0).toInt - 1))
      Triangle(v1, v2, v3)
    }
    val faces = fLines map readFace

    faces.toVector
  }
}
