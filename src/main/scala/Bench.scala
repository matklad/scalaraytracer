import components.geometry.Plain
import components.io.{DummyDisplay, ObjParser}

import components.primitives.{Chess, Primitive, Solid, Material}
import components.LightSource
import components.scene.SceneConfig
import data.Types._
import Color._
import scala.language.implicitConversions

object Bench extends DummyDisplay {
  def main(args: Array[String]) {
    val s = io.Source.fromFile("utah.obj").mkString
    val triangles = ObjParser.parse(s)
    val allPrimitives = for {t <- triangles} yield Primitive(t, Solid(white.amplify(.3)), Material.simple)
    val primitives = allPrimitives.take(400)
    val scene = SceneConfig(
      cameraPosition = P(0, 40, 90),
      up = -V.k,
      focus = 80,
      parallel = true,
      resolution = (640, 480),
      oversampling = 2,
      nReflections = 3,
      backgroundColor = blue.amplify(.2),
      ambientLight = white.amplify(.3)
    ).primitives(
        primitives: _*
      ).primitives(
        Primitive(Plain(P(0, -8, 0), ox = V.k, oy = V.i), Chess(side = 5, black = red), Material.simple)
      ).lights(
        LightSource(P(80, 80, 50), white),
        LightSource(P(80, 20, 10), red),
        LightSource(P(8, 80, 42), blue)
      ).scene()
    val start = System.currentTimeMillis()
    display(scene.render)
    val end = System.currentTimeMillis()
    println(s"time: ${(end - start) / 1000}s")
  }
}
