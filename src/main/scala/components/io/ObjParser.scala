package components.io

import components.geometry.{InterpolatedTriangle, Shape}
import data.Types._

object ObjParser {
  def parse(input: String): Vector[Shape] = {
    val lines = input.lines.toArray

    def selectLines(start: String): Array[String] =
      lines filter (_.startsWith(start + " ")) map (_.drop(start.length + 1).trim)

    val vLines = selectLines("v")
    val fLines = selectLines("f")
    val vnLines = selectLines("vn")

    def readVertex(line: String): P = {
      val Array(x, y, z) = line split " " map (_.toDouble)
      P(x, y, z)
    }
    val vertices = vLines map readVertex

    def readNormal(line: String): D = readVertex(line).v.direction
    val normals = vnLines map readNormal

    def readFace(line: String): InterpolatedTriangle = {
      def readBlock(block: String) = {
        val Array(vi, _, ni) = block.split("/") map (x => x.toInt - 1)
        (vertices(vi), normals(ni))
      }
      val blocks = line split " "

      val Array((v1, n1), (v2, n2), (v3, n3)) = blocks map readBlock
      InterpolatedTriangle(v1, n1, v2, n2, v3, n3)
      //      Triangle(v1, v2, v3)
    }
    val faces = fLines map readFace

    faces.toVector
  }
}
