import components.io.{ObjParser, ImageDisplay}

import components.{Material, Chess, Solid, LightSource}
import components.scene.SceneConfig
import components.shapes.{Plain, Sphere}
import data.Types._

import scala.language.implicitConversions

object Main extends ImageDisplay {
  implicit def TupleToPoint[A <% Double, B <% Double, C <% Double](t: (A, B, C)): P =
    P(t._1, t._2, t._3)

  def main(args: Array[String]) {
    val s = io.Source.fromFile("utah.obj").mkString
    val triangles = ObjParser.parse(s)
    val scene = SceneConfig(
      cameraPosition = (200, 0, 100),
      center = (0, 0, 15),
      up = V.k,
      focus = 80,
      parallel = true,
      oversampling = 3,
      nReflections = 3,
      backgroundColor = Color.blue.amplify(.2)
    ).shapes(
        Sphere(10, (-10, 10, 10), Solid(Color.white), Material.mirror),
        Sphere(10, (10, -20, 10), Solid(Color.white), Material.badMirror),
        Plain(texture = Chess(side = 20, black = Color.red))
      ).lights(
        LightSource((80, 80, 50), Color.red + Color.blue),
        LightSource((-20, -20, 150), Color.blue + Color.green)
      ).scene()
    display(scene.render)
    println("Done!")
  }
}
