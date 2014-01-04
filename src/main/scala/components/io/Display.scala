package components.io

import data.{Data, BitMap}

trait Display extends Data {
  def display(bitmap: BitMap): Unit
}
