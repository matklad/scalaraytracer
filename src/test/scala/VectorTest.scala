import data.Types._
import org.scalatest._

class VectorTest extends FunSuite {
  val a = V(1, 2, 3)
  val b = V(1, 2, 3)
  val sum = a + b
  val diff = a - b
  val scaled = a * 2
  val expected = V(2, 4, 6)
  test("addition") {
    assert(sum == expected)
  }
  test("difference") {
    assert(diff == V.zero)
  }
  test("scaling") {
    assert(scaled == expected)
  }
  test("dot") {
    assert(a.dot(b) == 14.0)
  }
  test("cross") {
    assert(a.cross(b) == V.zero)
  }
}
