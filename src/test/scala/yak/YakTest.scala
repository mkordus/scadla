import org.scalatest.flatspec.AnyFlatSpec
import scadla._
import scadla.EverythingIsIn.{millimeters, degrees}
import InlineOps._

class YakTest extends AnyFlatSpec {

  it should "render" in renderTest {
    val borderWidth = 2.5
    val keyWidth = 14.0
    val keyWithBorderWidth = keyWidth + borderWidth * 2
    val keyBorder = drawKeyBorder(
      borderWidth = borderWidth
    )

    Union(
      keyBorder.moveY(keyWithBorderWidth * 0),
      keyBorder.moveY(keyWithBorderWidth * 1),
      keyBorder.moveY(keyWithBorderWidth * 2),
      keyBorder.moveY(keyWithBorderWidth * 3)
    )
  }

  def drawKeyBorder(
      borderWidth: Double = 2.5,
      wallWidth: Double = 2.0,
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
        .moveZ(-wallWidth / 2)

    Union(
      borderPart.rotateZ(0),
      borderPart.rotateZ(90),
      borderPart.rotateZ(180),
      borderPart.rotateZ(270)
    )
  }

  private def renderTest(obj: Solid): Unit = {
    val renderingOption = List("$fn=100;")
    val renderer = new backends.OpenSCAD(renderingOption)
    // renderer.view(obj)
    renderer.toSTL(obj, "v1.stl")
  }
}
