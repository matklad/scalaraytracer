import components.shapes.Sphere
import data.Data
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{PropSpec, ShouldMatchers}
import org.scalacheck.{Arbitrary, Gen}

class ShapeTest extends PropSpec with GeneratorDrivenPropertyChecks with ShouldMatchers with Data {
  val vectors: Gen[V] = for {
    x <- Gen.choose(-100, 100)
    y <- Gen.choose(-100, 100)
    z <- Gen.choose(-100, 100)
  } yield
    V(x, y, z)

  val points = for {
    v <- vectors
  } yield P(v)

  val rays = for {
    p <- points
    v <- vectors
  } yield R(p, v.direction)
  implicit val arbRays = Arbitrary(rays)

  val spheres = for {
    p <- points
    r <- Gen.choose(1, 100)
  } yield Sphere(r, p)

  implicit val arbSpheres = Arbitrary(spheres)

  property("sphere intersection lies on sphere") {
    forAll {
      (s: Sphere, r: R) => {
        val t = s.intersectWith(r)
        val p = r.along(t)
        assert(t >= 0 && (t == S.MaxValue || math.abs((s.center - p).length - s.radius) < 1e-6))
      }
    }
  }
}
