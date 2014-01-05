package components.scene

class Options private(val resolution: (Int, Int), val oversampling: Int) {


}

object Options {
  def apply(resolution: (Int, Int) = (640, 480), oversampling: Int = 2) =
    new Options(resolution, oversampling)

  val default = Options()
}
