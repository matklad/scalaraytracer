package components.io

import components.shapes.{Triangle, Shape}
import data.Types._
import components.Material
import scala.util.Random

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

    def color(): Color = {
      Random.shuffle(List(Color.red, Color.blue, Color.green)).head
    }

    def readFace(line: String): Triangle = {
      val blocks = line split " "

      val Array(v1, v2, v3) = blocks map (x => vertices(x.split("/")(0).toInt - 1))
      Triangle(v1, v2, v3, Material(color()))
    }
    val faces = fLines map readFace

    faces.toVector
  }
}
