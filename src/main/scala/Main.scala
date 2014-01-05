import components.io.{ObjParser, ImageDisplay}

import components.{Material, LightSource}
import components.scene.{SceneBuilder, Camera}
import components.shapes.Sphere
import data.Types._

import scala.language.implicitConversions

object Main extends ImageDisplay {
  implicit def TupleToPoint[A <% Double, B <% Double, C <% Double](t: (A, B, C)): P =
    P(t._1, t._2, t._3)

  def main(args: Array[String]) {
    val s = io.Source.fromFile("utah.obj").mkString
    val triangles = ObjParser.parse(s)
    val scene = SceneBuilder(
      Camera(
        position = (50, 50, 80),
        up = V.j,
        focus = 80,
        width = 40,
        height = 30,
        resolutionX = 640 * 2,
        resolutionY = 480 * 2))
      .shapes(
        triangles: _*
      ).ambient(
        Color.white.amplify(.5)
      ).lights(
        LightSource((100, 0, 100), Color.white),
        LightSource((-100, 0, 100), Color.white)
      ).build()
    display(scene.render.downSample)
    println("Done!")
  }

  val canonicalScene = SceneBuilder(
    Camera(
      position = (50, 50, 80),
      focus = 80,
      width = 40,
      height = 30,
      resolutionX = 640,
      resolutionY = 480
    )).shapes(
      Sphere(8, (10, 0, 0), Material(Color(.7, 0, 0))),
      Sphere(8, (0, 10, 0), Material(Color(0, .7, 0)))
    ).ambient(
      Color(.09, .09, .09)
    )
    .lights(
      LightSource((100, 0, 100), Color(.9, .9, .9)),
      LightSource((-100, 0, 100), Color(.9, .9, .9))
    ).build()
}
