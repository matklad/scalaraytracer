package components.scene


import data.Data._
import components.{LightRay, Material, LightSource}
import components.shapes.{Sphere, Shape}

class Scene(val camera: Camera, val color: Color, val ambientLight: Color,
            val shapes: Vector[Shape], val lightSources: Vector[LightSource]) {

  def render: BitMap = {
    val start = System.currentTimeMillis()
    val bm = BitMap(camera.resolutionX, camera.resolutionY) {
      (x, y) =>
        val r = camera.apply(x, y)
        trace(r).norm
    }
    val end = System.currentTimeMillis()
    println((end - start) / 1000.0)
    bm
  }

  def shade(shape: Shape, point: P, view: D): Color = {
    val n = shape.normalAt(point)
    val p = point + n * 1e-6
    val m = shape.material
    val baseColor = m.color
    val lights = lightSources map (_.shed(p))
    val visibleLights = lights filter isVisible(point)

    val ambient = (baseColor * ambientLight).amplify(m.ambientK)
    var diffuse, specular = Color.zero
    visibleLights foreach {
      l =>
        val nz: Double => Double = math.max(0, _)
        val diffuseK = nz(l.ray.direction dot -n) * m.diffuseK
        diffuse += (baseColor * l.color).amplify(diffuseK)

        val base = nz(reflect(view, n) dot l.ray.direction)
        val specularK = math.pow(base, m.specularK)
        specular += l.color.amplify(specularK)
    }

    specular + ambient + diffuse
  }

  def reflect(original: D, n: D) = {
    (n * (n dot original)) * 2 - original
  }

  def isVisible(p: P)(l: LightRay): Boolean = {
    val (t, _) = intersect(l.ray)
    t > (p - l.ray.origin).length
  }

  def trace(ray: R): Color = {
    val (t, s) = intersect(ray)
    val p = ray.along(t)
    if (s != box)
      shade(s, p, ray.direction)
    else
      Color.pureBlack
  }

  def intersect(ray: R): (Double, Shape) = {
    var intersection: (Double, Shape) = (box.intersectWith(ray), box)
    shapes foreach {
      s =>
        val t = s.intersectWith(ray)
        if (t < intersection._1)
          intersection = (t, s)
    }
    intersection
  }

  object box extends Sphere(1e5, P.origin, Material.absoluteBlack) {
    override def normalAt(p: P): D = -super.normalAt(p)
  }

}

class SceneBuilder(val camera: Camera) {
  private val _color = Color.black
  private var _ambientLight = Color.white
  private var _shapes: Vector[Shape] = Vector()
  private var _lightSources: Vector[LightSource] = Vector()

  def shapes(shapes: Shape*): SceneBuilder = {
    _shapes ++= shapes
    this
  }

  def lights(lights: LightSource*): SceneBuilder = {
    _lightSources ++= lights
    this
  }

  def ambient(color: Color): SceneBuilder = {
    _ambientLight = color
    this
  }

  def build(): Scene = {
    new Scene(camera, _color, _ambientLight, _shapes, _lightSources)
  }
}

object SceneBuilder {
  def apply(camera: Camera): SceneBuilder = {
    new SceneBuilder(camera)
  }
}