package yak

import org.scalatest.flatspec.AnyFlatSpec
import scadla._
import scadla.EverythingIsIn.{millimeters, degrees}
import InlineOps._

class PocketBookHolderTest extends AnyFlatSpec {

  it should "render" in renderTest {
    val readerWidth = 7.4
    val holderWallWidth = 3.0
    val holderWidth = 25.0
    val holderHeight = 9.0
    val handleHeight = 90.0

    val base = Cube(
      width = holderWidth * 3,
      depth = readerWidth,
      height = holderWallWidth
    )
      .moveX(-holderWidth)

    val backWall = Cube(
      width = holderWidth * 3,
      depth = holderWallWidth,
      height = holderHeight * 2 + holderWallWidth
    )
      .moveY(-holderWallWidth)
      .moveX(-holderWidth)

    val handleRadius = 5.0

    val newHandle = Cylinder(
      radius = handleRadius,
      height = handleHeight
    )
      .moveX(holderWidth / 2)
      .moveY(handleRadius / 2)
      .moveZ(-handleHeight)

    val frontWall = Cube(
      width = holderWidth,
      depth = holderWallWidth,
      height = holderHeight + holderWallWidth
    )
      .moveY(readerWidth)

    val frontWallLeft = frontWall.moveX(-holderWidth)
    val frontWallRight = frontWall.moveX(holderWidth)

    Union(
      base,
      backWall,
      newHandle,
      frontWallLeft,
      frontWallRight
    ).rotateY(180)
  }

  private def renderTest(obj: Solid): Unit = {
    val renderingOption = List("$fn=100;")
    val renderer = new backends.OpenSCAD(renderingOption)
    renderer.view(obj)
    // renderer.toSTL(obj, "holder.stl")
  }
}
