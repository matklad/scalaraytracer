package simplex

class Dictionary private(private val m: Array[Array[Double]]) {

  private val h = m.size
  private val w = m(0).size

  assert(m.forall(_.size == w))

  def target: Double =
    m(0)(0)

  def magicPivot: Dictionary = {
    val enter = 1
    val leave = (1 until w).minBy(m(_)(0))
    if (m(leave)(0) < -1e-6)
      pivot(enter, leave)
    else
      this
  }

  def opt: Dictionary = {
    entering match {
      case None => this
      case Some(j) => pivot(j, leaving(j)).opt
    }
  }

  private def entering: Option[Int] =
    (1 until w).find(m(0)(_) > 1e-6)


  private def leaving(entering: Int) = {
    (1 until w)
      .filter(m(_)(entering) < 0)
      .minBy(i => -m(i)(0) / m(i)(entering))
  }

  private def pivot(enter: Int, leave: Int): Dictionary = {
    val rowK = m(leave)(enter)
    val row = m(leave) map (_ / -rowK)
    row(enter) = 1 / rowK

    val n = Array.ofDim[Double](h, w)
    for (i <- 0 until h if i != leave) {
      val k = m(i)(enter)
      n(i) = (m(i), row).zipped.map(_ + _ * k)
      n(i)(enter) = k / rowK
    }
    n(leave) = row

    new Dictionary(n)
  }


  override def toString = {
    def mkRow(row: Array[Double]): String = {
      row.map(d => f"$d%2.2f ").mkString(" ")
    }
    (m map mkRow).mkString("\n")
  }

}

object Dictionary {
  def apply(h: Int, w: Int, elems: Double*) = {
    assert(elems.size == h * w)
    val m = Array.ofDim[Double](h, w)
    for ((e, i) <- elems.zipWithIndex) {
      m(i / w)(i % w) = e
    }
    new Dictionary(m)
  }
}
