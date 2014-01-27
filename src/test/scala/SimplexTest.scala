import org.scalatest._
import simplex.Dictionary

class SimplexTest extends FunSuite {
  test("pivot") {
    val d = Dictionary(4, 4,
      0, 1, -1, -1,
      1, -1, 1, 1,
      2, -1, 2, 1,
      3, -1, 2, 2
    )
    val opt = d.opt
    assert(math.abs(opt.target - 1) < 1e-6)
  }

  test("opt") {
    val d = Dictionary(3, 3,
      0, 1, 2,
      3, -1, -1,
      4, -2, 3
    )
    assert(math.abs(d.opt.target - 6) < 1e-6)
  }
}
