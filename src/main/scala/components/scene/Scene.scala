package components.scene


import data.Types._
import components.{LightRay, LightSource}
import components.geometry.Sphere
import data.Ray
import components.primitives.{Primitive, Solid, Material}

class Scene private[scene](config: SceneConfig) {

  private val (resX, resY) = config.resolution
  private val oversampling = config.oversampling
  private val parallel = config.parallel
  private val backgroundColor = config.backgroundColor
  private val lightSources = config._lights
  private val ambientLight = config.ambientLight
  private val nReflections = config.nReflections
  private val threshold = 0.01
  private val primitives = config._primitives

  private val camera =
    Camera(config.cameraPosition, config.center, config.up, config.focus,
      config.screenSize, (resX * oversampling, resY * oversampling))


  def render: BitMap = {
    val start = System.currentTimeMillis()
    val os = oversampling
    val pixels = Array.fill[Array[Color]](resX)(null)
    val cols = 0 until resX
    for (x <- if (parallel) cols.par else cols) {
      pixels(x) = Array.tabulate[Color](resY) {
        y =>
          var c = Color.zero
          for (i <- 0 until os; j <- 0 until os)
            c += trace(camera.apply(os * x + i, os * y + j), nReflections)
          c.amplify(1.0 / (os * os))
      }
    }

    val bitMap = BitMap(resX, resY) {
      (x, y) => pixels(x)(y)
    }
    val end = System.currentTimeMillis()
    println(s"time for frame: ${(end - start) / 1000}s")
    bitMap
  }

  private def trace(ray: R, refN: Int = nReflections, impact: Double = 1): Color = {
    val (t, primitive) = intersect(ray)
    if (primitive.eq(box))
      backgroundColor
    else {
      val p = ray.along(t)
      val normal = primitive.normalAt(p)
      val color = primitive.colorAt(p)
      val moved = p + normal * 1e-6
      val ans = shade(moved, normal, ray.direction, color, primitive.material)
        .amplify(primitive.material.opacityK)
      val newImpact = impact * primitive.material.reflectK
      if (refN == 0 || newImpact < threshold)
        ans
      else {
        val reflectedRay = Ray(moved, reflect(ray.direction, normal).direction)
        val refColor = trace(reflectedRay, refN - 1, newImpact)
        ans + refColor.amplify(primitive.material.reflectK)
      }
    }
  }

  private def shade(point: P, normal: D, view: D, baseColor: Color, material: Material): Color = {
    val lights = lightSources map (_.shed(point))
    val visibleLights = lights filter isVisible(point)

    val ambient = (baseColor * ambientLight).amplify(material.ambientK)
    var diffuse, specular = Color.zero

    visibleLights foreach {
      l =>
        val nz: S => S = math.max(0, _)
        val diffuseK = nz(l.ray.direction dot -normal) * material.diffuseK
        diffuse += (baseColor * l.color).amplify(diffuseK)

        val base = nz(-reflect(view, normal) dot l.ray.direction)
        val specularK = math.pow(base, material.specularK)
        specular += l.color.amplify(specularK)
    }

    specular + ambient + diffuse
  }

  private def intersect(ray: R): (S, Primitive) = {
    var intersection: (S, Primitive) = (box.intersectWith(ray), box)
    primitives foreach {
      s =>
        val t = s.intersectWith(ray)
        if (t < intersection._1)
          intersection = (t, s)
    }
    intersection
  }

  private def reflect(original: D, n: D) = {
    original - (n * (n dot original)) * 2
  }

  private def isVisible(p: P)(l: LightRay): Boolean = {
    val (t, _) = intersect(l.ray)
    t > (p - l.ray.origin).length
  }

  private val box = Primitive(Sphere(1e10, P.origin), Solid(Color.pureBlack),
    Material.absoluteBlack)

}

class SceneConfig private(val cameraPosition: P,
                          val center: P,
                          val up: D,
                          val focus: S,
                          val screenSize: (S, S),
                          val resolution: (Int, Int),
                          val backgroundColor: Color,
                          val ambientLight: Color,
                          val oversampling: Int,
                          val parallel: Boolean,
                          val nReflections: Int
                           ) {
  private[scene] var _lights: Vector[LightSource] = Vector()
  private[scene] var _primitives: Vector[Primitive] = Vector()

  def lights(lights: LightSource*): SceneConfig = {
    this._lights = lights.toVector
    this
  }

  def primitives(primitives: Primitive*): SceneConfig = {
    this._primitives ++= primitives.toVector
    this
  }

  def scene(): Scene = new Scene(this)
}

object SceneConfig {
  def apply(cameraPosition: P = P(50, 0, 50),
            center: P = P.origin,
            up: D = V.k,
            focus: S = 50,
            screenSize: (S, S) = (40, 30),
            resolution: (Int, Int) = (640, 480),
            backgroundColor: Color = Color.black,
            ambientLight: Color = Color.white.amplify(.3),
            oversampling: Int = 2,
            parallel: Boolean = true,
            nReflections: Int = 0) = {
    new SceneConfig(cameraPosition, center, up, focus, screenSize, resolution,
      backgroundColor, ambientLight, oversampling, parallel, nReflections)
  }
}