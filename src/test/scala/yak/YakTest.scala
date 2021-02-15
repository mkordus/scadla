import org.scalatest.flatspec.AnyFlatSpec
import scadla._
import scadla.EverythingIsIn.{millimeters, degrees}
import InlineOps._

class YakTest extends AnyFlatSpec {

  it should "render" in renderTest {
    val borderWidth = 2.5
    val keyWidth = 14.0
    val keyWithBorderWidth = keyWidth + borderWidth * 2
    val key = drawKeyBorder(
      borderWidth = borderWidth
    )
    val column = drawColumn(keyWithBorderWidth, key, _)

    Union(
      key.moveX(keyWithBorderWidth * -1).moveY(-6.0 - keyWithBorderWidth),
      column(5).moveX(keyWithBorderWidth * 0).moveY(0 - keyWithBorderWidth),
      column(5).moveX(keyWithBorderWidth * 1).moveY(3.0 - keyWithBorderWidth),
      column(4).moveX(keyWithBorderWidth * 2).moveY(5.0),
      column(4).moveX(keyWithBorderWidth * 3).moveY(3.0),
      column(4).moveX(keyWithBorderWidth * 4).moveY(-2.0),
      column(3).moveX(keyWithBorderWidth * 5).moveY(-3.0)
    )
  }

  def drawColumn(
      keyWithBorderWidth: Double,
      keyBorder: Solid,
      numberOfKeys: Int
  ): Solid = {
    val columnBorders = for {
      i <- 0 to numberOfKeys - 1
    } yield keyBorder.moveY(keyWithBorderWidth * i)

    Union(columnBorders: _*)
  }

  def drawKeyBorder(
      borderWidth: Double = 2.5,
      wallWidth: Double = 4.0,
      keyWidth: Double = 14.0
  ) = {
    val borderCubeWidth = keyWidth + (borderWidth * 2)

    val borderPart =
      Cube(
        width = borderCubeWidth,
        depth = borderWidth,
        height = wallWidth
      )
        .moveX(-borderCubeWidth / 2)
        .moveY(-borderCubeWidth / 2)

    val gapDepth = 1.0
    val gapWidth = 5.0

    val gap =
      Cube(
        width = gapWidth,
        depth = gapDepth,
        height = wallWidth - 2.0
      )
        .moveX(-gapWidth / 2)
        .moveY((-keyWidth - gapDepth) / 2)

    val borders = Union(
      borderPart.rotateZ(0),
      borderPart.rotateZ(90),
      borderPart.rotateZ(180),
      borderPart.rotateZ(270)
    )

    val gaps = Union(
      gap.rotateZ(0),
      gap.rotateZ(180)
    )

    Difference(borders, gaps)
  }

  private def renderTest(obj: Solid): Unit = {
    val renderingOption = List("$fn=100;")
    val renderer = new backends.OpenSCAD(renderingOption)
    // renderer.view(obj)
    renderer.toSTL(obj, "v1.stl")
  }
}
