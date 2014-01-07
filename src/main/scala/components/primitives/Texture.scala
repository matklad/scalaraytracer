package components.primitives

import data.Types._


trait Texture {
  def colorAt(x: S, y: S): Color
}

class Solid private(private val color: Color) extends Texture {
  def colorAt(x: S, y: S): Color = color
}

object Solid {
  def apply(color: Color) = new Solid(color)
}

class Chess private(private val side: S, private val black: Color, private val white: Color) extends Texture {
  def colorAt(x: S, y: S): Color =
    if (((x / side).floor + (y / side).floor) % 2 == 0) black else white
}

object Chess {
  def apply(side: S = 1, black: Color = Color.black, white: Color = Color.white) =
    new Chess(side, black, white)
}