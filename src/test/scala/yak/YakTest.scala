package yak

import org.scalatest.flatspec.AnyFlatSpec
import scadla._
import scadla.EverythingIsIn.{millimeters, degrees}
import InlineOps._

class YakTest extends AnyFlatSpec {

  it should "render" in renderTest {
    val spacing = 19.0

    val layout = Layout(
      Key
        .move(x = spacing * -1, y = -6.0 - spacing),
      Column
        .numberOfKeys(5)
        .move(x = spacing * 0, y = -spacing),
      Column
        .numberOfKeys(5)
        .move(x = spacing * 1, y = 3.0 - spacing),
      Column
        .numberOfKeys(4)
        .move(x = spacing * 2, y = 5.0),
      Column
        .numberOfKeys(4)
        .move(x = spacing * 3, y = 3.0),
      Column
        .numberOfKeys(4)
        .move(x = spacing * 4, y = -2.0),
      Column
        .numberOfKeys(3)
        .move(x = spacing * 5, y = -4.0)
    )

    layout.render
  }

  private def renderTest(obj: Solid): Unit = {
    val renderingOption = List("$fn=100;")
    val renderer = new backends.OpenSCAD(renderingOption)
    renderer.view(obj)
    // renderer.toSTL(obj, "v1.stl")
  }
}
