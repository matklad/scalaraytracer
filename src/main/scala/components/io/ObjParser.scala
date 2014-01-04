package components.io

import components.shapes.{Triangle, Shape}
import data.Types._
import components.Material

object ObjParser {
  def parse(input: String): Vector[Shape] = {
    val lines = input.lines.toArray
    val vLines = lines filter (_.startsWith("v "))
    val fLines = lines filter (_.startsWith("f "))

    def readVertex(line: String): P = {
      val Array(x, y, z) = line split " " drop 2 map (_.toDouble)
      P(x, y, z)
    }
    val verts = (vLines map readVertex).toVector

    def readFace(line: String): Triangle = {
      val blocks = line split " " drop 1

      val Array(v1, v2, v3) = blocks map (x => verts(x.split("/")(0).toInt - 1))
      Triangle(v1, v2, v3, Material(Color.red))
    }
    val faces = (fLines map readFace).toVector

    faces
  }
}
