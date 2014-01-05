package components.io

import javax.swing.{ImageIcon, JLabel, JFrame}
import java.awt.{Image, FlowLayout}
import java.awt.image.BufferedImage
import data.Types._

class ImageDisplay extends Display {

  def display(bitmap: BitMap): Unit = {
    val image = mkImage(bitmap)
    displayImage(image)
  }

  private def mkImage(bitmap: BitMap): Image = {
    val image = new BufferedImage(bitmap.width, bitmap.height, BufferedImage.TYPE_INT_ARGB)
    val raster = image.getRaster
    for (x <- 0 until bitmap.width;
         y <- 0 until bitmap.height)
      bitmap(x, y) match {
        case Color.RGB(r, g, b) =>
          raster.setPixel(x, y, Array[Double](r, g, b, 255))
      }

    image
  }

  private def displayImage(image: Image): Unit = {
    val frame = new JFrame()
    frame.getContentPane.setLayout(new FlowLayout())
    frame.getContentPane.add(new JLabel(new ImageIcon(image)))
    frame.pack()
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)
  }
}
