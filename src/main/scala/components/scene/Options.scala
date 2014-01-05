package components.scene

class Options private(val resolution: (Int, Int), val oversampling: Int, val parallel: Boolean) {


}

object Options {
  def apply(resolution: (Int, Int) = (640, 480), oversampling: Int = 2,
            parallel: Boolean = true) =
    new Options(resolution, oversampling, parallel)

  val default = Options()
}
