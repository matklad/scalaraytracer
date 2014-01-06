import components.io.{ObjParser, ImageDisplay}

import components.LightSource
import components.scene.SceneConfig
import data.Types._

import scala.language.implicitConversions

object Main extends ImageDisplay {
  implicit def TupleToPoint[A <% Double, B <% Double, C <% Double](t: (A, B, C)): P =
    P(t._1, t._2, t._3)

  def main(args: Array[String]) {
    val s = io.Source.fromFile("utah.obj").mkString
    val triangles = ObjParser.parse(s)
    val scene = SceneConfig(
      cameraPosition = (50, 50, 80),
      up = V.j,
      focus = 80
    ).shapes(
        triangles: _*
      ).lights(
        LightSource((100, 0, 100), Color.white),
        LightSource((-100, 0, 100), Color.white)
      ).scene()
    display(scene.render)
    println("Done!")
  }
}
