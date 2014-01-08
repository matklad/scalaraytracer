import components.geometry.Plain
import components.io.{ObjParser, ImageDisplay}

import components.primitives.{Chess, Primitive, Solid, Material}
import components.LightSource
import components.scene.SceneConfig
import data.Types._
import Color._
import scala.language.implicitConversions

object Main extends ImageDisplay {
  implicit def TupleToPoint[A <% Double, B <% Double, C <% Double](t: (A, B, C)): P =
    P(t._1, t._2, t._3)

  def main(args: Array[String]) {
    val s = io.Source.fromFile("utah.obj").mkString
    val triangles = ObjParser.parse(s)
    val primitives = for {t <- triangles} yield Primitive(t, Solid(white.amplify(.3)), Material.simple)
    val scene = SceneConfig(
      cameraPosition = (0, 10, 90),
      up = -V.k,
      focus = 80,
      parallel = true,
      resolution = (640, 480),
      oversampling = 2,
      nReflections = 1,
      backgroundColor = blue.amplify(.2),
      ambientLight = white.amplify(.3)
    ).primitives(
        primitives: _*
      ).primitives(
        Primitive(Plain((0, -8, 0), ox = V.k, oy = V.i), Chess(side = 5, black = red), Material.simple)
      ).lights(
        LightSource((80, 80, 50), white),
        LightSource((-20, 20, 150), blue + green)
      ).scene()
    display(scene.render)
    println("Done!")
  }
}
